package undersampling.core

import undersampling.io.Logger
import undersampling.util.Utilities._

/** Implementation of the algorithms
  *
  * @param x             data to work with
  * @param y             labels of the data associated with x
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass integer representing the class to keep it untouched
  * @author Néstor Rodríguez Vico
  */
class CondensedNearestNeighbor(override private[undersampling] val x: Array[Array[Double]],
                               override private[undersampling] val y: Array[Int],
                               override private[undersampling] val seed: Long = System.currentTimeMillis(),
                               minorityClass: Int = 0) extends Algorithm(x, y, seed, minorityClass) {

  /** Compute the Condensed Nearest Neighbor decision rule (CNN rule)
    *
    * @param file     file to store the log
    * @param distance distance to use when calling the NNRule algorithm
    * @return reduced data, reduced labels, index of elements kept
    */
  def sample(file: String, distance: Distances.Distance): (Array[Array[Double]], Array[Int], Array[Int]) = {
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
      val label: Int = nnRule(data = index map this.normalizedData, labels = index map this.randomizedY,
        newInstance = element._1, newInstanceLabel = this.randomizedY(element._2), k = 1, distance = distance)
      // If it is no well classified or is a element of the minority class
      if (label != this.randomizedY(element._2) || this.randomizedY(element._2) == this.untouchableClass) {
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
        val label: Int = nnRule(data = index map this.normalizedData, labels = index map this.randomizedY,
          newInstance = this.normalizedData(element._2), newInstanceLabel = this.randomizedY(element._2), k = 1, distance = distance)
        // If it is no well classified or is a element of the minority class
        if (label != this.randomizedY(element._2) || this.randomizedY(element._2) == this.untouchableClass) {
          // it is added to store
          location(element._2) = 1
          changed = true
        } else {
          // otherwise, it is added to grabbag
          location(element._2) = -1
        }
      }

      logger.addMsg("Iteration %d: grabbag size: %d, store size: %d.".format(iteration, location.count((z: Int) => z == -1), location.count((z: Int) => z == 1)), 0)
    }

    // The final data is the content of store
    val storeIndex: Array[Int] = location.zipWithIndex.filter((x: (Int, Int)) => x._1 == 1).collect { case (_, a) => a }
    val store: Array[Array[Double]] = storeIndex map this.randomizedX
    val storeClasses: Array[Int] = storeIndex map this.randomizedY
    // Recount of classes
    val newCounter: Array[(Int, Int)] = storeClasses.groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray
    logger.info(0) += "\nNEW DATA SIZE: %d\n".format(storeIndex.length)
    logger.info(0) += "\nREDUCTION PERCENTAGE: %f\n".format(100 - (storeIndex.length.toFloat / this.randomizedX.length) * 100)
    // Recompute the Imbalanced Ratio
    logger.addMsg("NEW IMBALANCED RATIO: %s".format(((newCounter.map((_: (Int, Int))._2).sum.toFloat - this.minorityElements) / this.minorityElements).toString), 1)
    // Save the logs
    logger.storeFile(file + "_CNN")

    (store, storeClasses, storeIndex)
  }
}
