package undersampling.algorithm

import smile.data.AttributeDataset
import undersampling.io.Logger
import undersampling.util.Utilities._

/** Implementation of the Condensed Nearest Neighbor decision rule (CNN rule).
  *
  * @param data           data to work with
  * @param seed           seed to use. If it is not provided, it will use the system time
  * @param minority_class integer representing the class to keep it untouched
  * @author Néstor Rodríguez Vico
  */
class CNN(private[undersampling] val data: AttributeDataset, private[undersampling] val seed: Long = System.currentTimeMillis(),
          minority_class: Int = 0) {

  private[undersampling] val random = new util.Random(seed)
  // Shuffle the data to make it random
  private[undersampling] val index: List[Int] = this.random.shuffle((0 until data.data().size()).toList)
  private[undersampling] val x: Array[Array[Double]] = (this.index map data.toArray(new Array[Array[Double]](data.size))).toArray
  private[undersampling] val y: Array[Int] = (this.index map data.toArray(new Array[Int](data.size))).toArray
  // Count the number of instances for each class
  private[undersampling] val counter: Array[(Int, Int)] = this.y.groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray.sortBy { case (_, d) => d }
  // In certain algorithms, reduce the minority class is forbidden, so let's detect what class is it if the user don't
  // set one at pleasure
  private[undersampling] val untouchable_class: Int = if (minority_class == 0) this.counter.head._1 else minority_class
  // Extra information to obtain the Imbalanced Ratio
  private[undersampling] val minority_elements: Int = this.counter.head._2
  private[undersampling] val majority_elements: Int = this.counter.tail.map((_: (Int, Int))._2).sum
  // Info to normalize the data
  private[undersampling] val max_v: Array[Double] = this.x.transpose.map((x: Array[Double]) => x.max)
  private[undersampling] val min_v: Array[Double] = this.x.transpose.map((x: Array[Double]) => x.min)
  // Normalize the data as follow: for each column, x, (x-min(x))/(max(x)-min(x))
  private[undersampling] val normalized_data: Array[Array[Double]] = (this.x.transpose zip (min_v zip max_v)).map((row: (Array[Double], (Double, Double))) =>
    row._1.map((e: Double) => e - row._2._1 / (row._2._2 / row._2._1)))

  /** Compute the CNN algorithm
    *
    * @param file file to store the log
    * @return
    */
  def compute(file: String): AttributeDataset = {
    val logger: Logger = new Logger(numberLogs = 2)
    logger.info += "DATA SIZE REDUCTION INFORMATION. \nORIGINAL DATA SIZE: " + this.normalized_data.length.toString
    logger.info += "ORIGINAL IMBALANCED RATIO: " + (this.majority_elements.toFloat / this.minority_elements).toString

    // Indicate the corresponding group: 1 for store, 0 for unknown, -1 for grabbag
    val location: Array[Int] = List.fill(this.normalized_data.length)(0).toArray
    var iteration: Int = 0
    // The first element is added to store
    location(0) = 1
    var changed = true

    // Iterate the data, x (except the first instance)
    for (element <- this.normalized_data.zipWithIndex.slice(1, this.normalized_data.length)) {
      // and classify each element with the actual content of store
      val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
      val label: Int = nnRule(data = index map this.normalized_data, labels = index map this.y, newInstance = element._1, k = 1)
      // If it is no well classified or is a element of the minority class
      if (label != this.y(element._2) || this.y(element._2) == this.untouchable_class) {
        // it is added to store
        location(element._2) = 1
      } else {
        // otherwise, it is added to grabbag
        location(element._2) = -1
      }
    }

    logger.addMsg("Iteration " + iteration + ": grabbag size: " + location.count((z: Int) => z == -1) +
      ", store size: " + location.count((z: Int) => z == 1) + ".", 0)

    // After a first pass, iterate grabbag until is exhausted:
    // 1. There is no element in grabbag or
    // 2. There is no data change between grabbag and store after a full iteration
    while (location.count((z: Int) => z == -1) != 0 && changed) {
      iteration += 1
      changed = false
      // Now, instead of iterating x, we iterate grabbag
      for (element <- location.zipWithIndex.filter((x: (Int, Int)) => x._1 == -1)) {
        val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
        val label: Int = nnRule(data = index map this.normalized_data, labels = index map this.y, newInstance = this.normalized_data(element._2), k = 1)
        // If it is no well classified or is a element of the minority class
        if (label != this.y(element._2) || this.y(element._2) == this.untouchable_class) {
          // it is added to store
          location(element._2) = 1
        } else {
          // otherwise, it is added to grabbag
          location(element._2) = -1
        }
      }

      logger.addMsg("Iteration " + iteration + ": grabbag size: " + location.count((z: Int) => z == -1) +
        ", store size: " + location.count((z: Int) => z == 1) + ".", 0)
    }

    // The final data is the content of store
    val storeIndex: Array[Int] = location.zipWithIndex.filter((x: (Int, Int)) => x._1 == 1).collect { case (_, a) => a }
    val store: Array[Array[Double]] = storeIndex map this.x
    val storeClasses: Array[Int] = storeIndex map this.y
    // Recount of classes
    val newCounter: Array[(Int, Int)] = storeClasses.groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray
    logger.info(0) += "\nNEW DATA SIZE: " + storeIndex.length + "\n"
    logger.info(0) += "\nREDUCTION PERCENTAGE: " + (100 - (storeIndex.length.toFloat / this.x.length) * 100) + "\n"
    // Recompute the Imbalanced Ratio
    logger.addMsg("NEW IMBALANCED RATIO: " + ((newCounter.map((_: (Int, Int))._2).sum.toFloat - this.minority_elements) / this.minority_elements).toString, 1)
    // Save the logs
    logger.storeFile(file)

    // Return an AttributeDataset
    toDataSet(this.data, store, storeClasses)
  }
}
