package undersampling.core

import smile.data.AttributeDataset
import undersampling.io.Logger
import undersampling.util.Utilities._

/** Implementation of the algorithms
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass integer representing the class to keep it untouched
  * @author Néstor Rodríguez Vico
  */
class Algorithm(private[undersampling] val data: AttributeDataset, private[undersampling] val seed: Long = System.currentTimeMillis(),
                minorityClass: Int = 0) {

  private[undersampling] val random = new util.Random(seed)
  // Shuffle the data to make it random
  private[undersampling] val index: List[Int] = this.random.shuffle((0 until data.data().size()).toList)
  private[undersampling] val x: Array[Array[Double]] = (this.index map data.toArray(new Array[Array[Double]](data.size))).toArray
  private[undersampling] val y: Array[Int] = (this.index map data.toArray(new Array[Int](data.size))).toArray
  // Count the number of instances for each class
  private[undersampling] val counter: Array[(Int, Int)] = this.y.groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray.sortBy { case (_, d) => d }
  // In certain algorithms, reduce the minority class is forbidden, so let's detect what class is it if the user don't
  // set one at pleasure
  private[undersampling] val untouchableClass: Int = if (minorityClass == 0) this.counter.head._1 else minorityClass
  // Extra information to obtain the Imbalanced Ratio
  private[undersampling] val minorityElements: Int = this.counter.head._2
  private[undersampling] val majorityElements: Int = this.counter.tail.map((_: (Int, Int))._2).sum
  // Info to normalize the data
  private[undersampling] val maxV: Array[Double] = this.x.transpose.map((x: Array[Double]) => x.max)
  private[undersampling] val minV: Array[Double] = this.x.transpose.map((x: Array[Double]) => x.min)
  // Normalize the data as follow: for each column, x, (x-min(x))/(max(x)-min(x))
  private[undersampling] val normalizedData: Array[Array[Double]] = (this.x.transpose zip (minV zip maxV)).map((row: (Array[Double], (Double, Double))) =>
    row._1.map((e: Double) => e - row._2._1 / (row._2._2 / row._2._1))).transpose

  /** Compute a random undersampling. This algorithm preserve, at least, numberOfElements elements from the majority class
    *
    * @param file             file to store the log
    * @param numberOfElements number of elements to preserve from the majority class
    * @return AttributeDataset with the reduced data
    */
  def RandomUndersampling(file: String, numberOfElements: Int): AttributeDataset = {
    val logger: Logger = new Logger(numberLogs = 2)
    logger.info += "DATA SIZE REDUCTION INFORMATION. \nORIGINAL DATA SIZE: %s".format(this.normalizedData.length.toString)
    logger.info += "ORIGINAL IMBALANCED RATIO: %s".format((this.majorityElements.toFloat / this.minorityElements).toString)

    // The elements in the minority class (untouchableClass) are maintained
    val minorityIndex: Array[Int] = this.y.zipWithIndex.collect { case (label, i) if label == this.untouchableClass => i }
    // It is not possible to select more elements than the available
    val elementsToSelect: Int = if (numberOfElements > this.majorityElements) this.majorityElements else numberOfElements
    // Get the index of the elements in the majority class
    val majorityIndex: Array[Int] = random.shuffle(this.y.zipWithIndex.collect { case (label, i) if label != this.untouchableClass => i }.toList).toArray.slice(0, elementsToSelect)
    // Get the index of the reduced data
    val finalIndex: Array[Int] = minorityIndex ++ majorityIndex

    // Recount of classes
    val newCounter: Array[(Int, Int)] = (finalIndex map this.y).groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray
    logger.info(0) += "\nNEW DATA SIZE: %d\n".format(finalIndex.length)
    logger.info(0) += "\nREDUCTION PERCENTAGE: %f\n".format(100 - (finalIndex.length.toFloat / this.x.length) * 100)
    // Recompute the Imbalanced Ratio
    logger.addMsg("NEW IMBALANCED RATIO: %s".format(((newCounter.map((_: (Int, Int))._2).sum.toFloat - this.minorityElements) / this.minorityElements).toString), 1)
    // Save the logs
    logger.storeFile(file + "_RU")
    // Return an AttributeDataset
    toDataSet(this.data, finalIndex map this.x, finalIndex map this.y)
  }

  /** Compute the Condensed Nearest Neighbor decision rule (CNN rule)
    *
    * @param file     file to store the log
    * @param distance distance to use when calling the NNRule algorithm
    * @return AttributeDataset with the reduced data
    */
  def CNN(file: String, distance: Distances.Distance): AttributeDataset = {
    val logger: Logger = new Logger(numberLogs = 2)
    logger.info += "DATA SIZE REDUCTION INFORMATION. \nORIGINAL DATA SIZE: %s".format(this.normalizedData.length.toString)
    logger.info += "ORIGINAL IMBALANCED RATIO: %s".format((this.majorityElements.toFloat / this.minorityElements).toString)

    // Indicate the corresponding group: 1 for store, 0 for unknown, -1 for grabbag
    val location: Array[Int] = List.fill(this.normalizedData.length)(0).toArray
    var iteration: Int = 0
    // The first element is added to store
    location(0) = 1
    var changed = true

    // Iterate the data, x (except the first instance)
    for (element <- this.normalizedData.zipWithIndex.slice(1, this.normalizedData.length)) {
      // and classify each element with the actual content of store
      val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
      val label: Int = nnRule(data = index map this.normalizedData, labels = index map this.y,
        newInstance = element._1, newInstanceLabel = this.y(element._2), k = 1, distance = distance)
      // If it is no well classified or is a element of the minority class
      if (label != this.y(element._2) || this.y(element._2) == this.untouchableClass) {
        // it is added to store
        location(element._2) = 1
      } else {
        // otherwise, it is added to grabbag
        location(element._2) = -1
      }
    }

    logger.addMsg("Iteration %d: grabbag size: %d, store size: %d.".format(iteration, location.count((z: Int) => z == -1), location.count((z: Int) => z == 1)), 0)

    // After a first pass, iterate grabbag until is exhausted:
    // 1. There is no element in grabbag or
    // 2. There is no data change between grabbag and store after a full iteration
    while (location.count((z: Int) => z == -1) != 0 && changed) {
      iteration += 1
      changed = false
      // Now, instead of iterating x, we iterate grabbag
      for (element <- location.zipWithIndex.filter((x: (Int, Int)) => x._1 == -1)) {
        val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
        val label: Int = nnRule(data = index map this.normalizedData, labels = index map this.y,
          newInstance = this.normalizedData(element._2), newInstanceLabel = this.y(element._2), k = 1, distance = distance)
        // If it is no well classified or is a element of the minority class
        if (label != this.y(element._2) || this.y(element._2) == this.untouchableClass) {
          // it is added to store
          location(element._2) = 1
        } else {
          // otherwise, it is added to grabbag
          location(element._2) = -1
        }
      }

      logger.addMsg("Iteration %d: grabbag size: %d, store size: %d.".format(iteration, location.count((z: Int) => z == -1), location.count((z: Int) => z == 1)), 0)
    }

    // The final data is the content of store
    val storeIndex: Array[Int] = location.zipWithIndex.filter((x: (Int, Int)) => x._1 == 1).collect { case (_, a) => a }
    val store: Array[Array[Double]] = storeIndex map this.x
    val storeClasses: Array[Int] = storeIndex map this.y
    // Recount of classes
    val newCounter: Array[(Int, Int)] = storeClasses.groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray
    logger.info(0) += "\nNEW DATA SIZE: %d\n".format(storeIndex.length)
    logger.info(0) += "\nREDUCTION PERCENTAGE: %f\n".format(100 - (storeIndex.length.toFloat / this.x.length) * 100)
    // Recompute the Imbalanced Ratio
    logger.addMsg("NEW IMBALANCED RATIO: %s".format(((newCounter.map((_: (Int, Int))._2).sum.toFloat - this.minorityElements) / this.minorityElements).toString), 1)
    // Save the logs
    logger.storeFile(file + "_CNN")

    // Return an AttributeDataset
    toDataSet(this.data, store, storeClasses)
  }
}
