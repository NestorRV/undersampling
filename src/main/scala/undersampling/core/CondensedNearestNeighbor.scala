package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

/** Condensed Nearest Neighbor decision rule
  *
  * @param data data to work with
  * @param seed seed to use. If it is not provided, it will use the system time
  * @author Néstor Rodríguez Vico
  */
class CondensedNearestNeighbor(override private[undersampling] val data: Data,
                               override private[undersampling] val seed: Long = System.currentTimeMillis()) extends Algorithm(data, seed) {

  /** Compute the Condensed Nearest Neighbor decision rule (CNN rule)
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance): Data = {
    // Original paper: "The Condensed Nearest Neighbor Rule" by P. Hart.

    if (file.isDefined) {
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(this.randomizedX.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))
    }

    // Indicate the corresponding group: 1 for store, 0 for unknown, -1 for grabbag
    val location: Array[Int] = List.fill(this.randomizedX.length)(0).toArray
    var iteration: Int = 0
    // The first element is added to store
    location(0) = 1
    var changed = true

    // Iterate the data, x (except the first instance)
    for (element <- this.randomizedX.zipWithIndex.tail) {
      // and classify each element with the actual content of store
      val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
      val label: (Any, Option[Array[Int]]) = nnRule(data = index map this.randomizedX, labels = index map this.randomizedY,
        newInstance = element._1, newInstanceLabel = this.randomizedY(element._2), k = 1, distance = distance)
      // If it is no well classified or is a element of the minority class
      if (label._1 != this.randomizedY(element._2) || this.randomizedY(element._2) == this.untouchableClass) {
        // it is added to store
        location(element._2) = 1
      } else {
        // otherwise, it is added to grabbag
        location(element._2) = -1
      }
    }

    if (file.isDefined) {
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "Iteration %d: grabbag size: %d, store size: %d.".format(iteration,
        location.count((z: Int) => z == -1), location.count((z: Int) => z == 1)))
    }

    // After a first pass, iterate grabbag until is exhausted:
    // 1. There is no element in grabbag or
    // 2. There is no data change between grabbag and store after a full iteration
    while (location.count((z: Int) => z == -1) != 0 && changed) {
      iteration += 1
      changed = false
      // Now, instead of iterating x, we iterate grabbag
      for (element <- location.zipWithIndex.filter((x: (Int, Int)) => x._1 == -1)) {
        val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
        val label: (Any, Option[Array[Int]]) = nnRule(data = index map this.randomizedX, labels = index map this.randomizedY,
          newInstance = this.randomizedX(element._2), newInstanceLabel = this.randomizedY(element._2), k = 1, distance = distance)
        // If it is no well classified or is a element of the minority class
        if (label._1 != this.randomizedY(element._2) || this.randomizedY(element._2) == this.untouchableClass) {
          // it is added to store
          location(element._2) = 1
          changed = true
        } else {
          // otherwise, it is added to grabbag
          location(element._2) = -1
        }
      }

      if (file.isDefined) {
        this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "Iteration %d: grabbag size: %d, store size: %d.".format(iteration,
          location.count((z: Int) => z == -1), location.count((z: Int) => z == 1)))
      }
    }

    // The final data is the content of store
    val storeIndex: Array[Int] = location.zipWithIndex.filter((x: (Int, Int)) => x._1 == 1).collect { case (_, a) => a }

    if (file.isDefined) {
      // Recount of classes
      val newCounter: Array[(Any, Int)] = (storeIndex map this.randomizedY).groupBy(identity).mapValues((_: Array[Any]).length).toArray
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(storeIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (storeIndex.length.toFloat / this.randomizedX.length) * 100).toString)
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO", "NEW: %s".format(imbalancedRatio(newCounter)))
      // Save the log
      this.logger.storeFile(file.get + "_CNN")
    }

    this.data._resultData = (storeIndex map this.index).sorted map this.data._originalData
    this.data._resultClasses = (storeIndex map this.index).sorted map this.data._originalClasses
    this.data._index = (storeIndex map this.index).sorted

    this.data
  }
}
