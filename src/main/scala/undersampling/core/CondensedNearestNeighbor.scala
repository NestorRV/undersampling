package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

/** Condensed Nearest Neighbor decision rule. Original paper: "The Condensed Nearest Neighbor Rule" by P. Hart.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class CondensedNearestNeighbor(override private[undersampling] val data: Data,
                               override private[undersampling] val seed: Long = System.currentTimeMillis(),
                               override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** Compute the Condensed Nearest Neighbor decision rule (CNN rule)
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance): Data = {
    // Use normalized data for EUCLIDEAN distance and randomized data
    val dataToWorkWith: Array[Array[Double]] = if (distance == Distances.EUCLIDEAN)
      (this.index map zeroOneNormalization(this.data)).toArray else
      (this.index map this.x).toArray

    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    if (file.isDefined) {
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE", "DISTANCES CALCULATION TIME", "TIME"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(dataToWorkWith.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter, this.untouchableClass)))
    }

    // Start the time
    val initTime: Long = System.nanoTime()

    val initDistancesTime: Long = System.nanoTime()
    // Distances among the elements
    val distances: Array[Array[Double]] = computeDistances(dataToWorkWith, distance, this.data._nominal, this.y)
    val distancesTime: Long = System.nanoTime() - initDistancesTime

    // Indicate the corresponding group: 1 for store, 0 for unknown, -1 for grabbag
    val location: Array[Int] = List.fill(dataToWorkWith.length)(0).toArray
    var iteration: Int = 0
    // The first element is added to store
    location(0) = 1
    var changed = true

    // Iterate the data, x (except the first instance)
    dataToWorkWith.zipWithIndex.tail.foreach { element: (Array[Double], Int) =>
      // and classify each element with the actual content of store
      val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
      val label: (Any, Array[Int]) = nnRule(distances = distances(element._2), selectedElements = index, labels = classesToWorkWith, k = 1)
      // If it is misclassified or is a element of the untouchable class it is added to store; otherwise, it is added to grabbag
      location(element._2) = if (label._1 != classesToWorkWith(element._2) || classesToWorkWith(element._2) == this.untouchableClass) 1 else -1
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
      location.zipWithIndex.filter((x: (Int, Int)) => x._1 == -1).foreach { element: (Int, Int) =>
        val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
        val label: (Any, Array[Int]) = nnRule(distances = distances(element._2), selectedElements = index, labels = classesToWorkWith, k = 1)
        // If it is misclassified or is a element of the untouchable class it is added to store; otherwise, it is added to grabbag
        location(element._2) = if (label._1 != classesToWorkWith(element._2) || classesToWorkWith(element._2) == this.untouchableClass) {
          changed = true
          1
        } else -1
      }

      if (file.isDefined) {
        this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "Iteration %d: grabbag size: %d, store size: %d.".format(iteration,
          location.count((z: Int) => z == -1), location.count((z: Int) => z == 1)))
      }
    }

    // The final data is the content of store
    val storeIndex: Array[Int] = location.zipWithIndex.filter((x: (Int, Int)) => x._1 == 1).collect { case (_, a) => a }

    // Stop the time
    val finishTime: Long = System.nanoTime()

    this.data._resultData = (storeIndex map this.index).sorted map this.data._originalData
    this.data._resultClasses = (storeIndex map this.index).sorted map this.data._originalClasses
    this.data._index = (storeIndex map this.index).sorted

    if (file.isDefined) {
      // Recount of classes
      val newCounter: Map[Any, Int] = (storeIndex map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length)
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(storeIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (storeIndex.length.toFloat / dataToWorkWith.length) * 100).toString)
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO", "NEW: %s".format(imbalancedRatio(newCounter, this.untouchableClass)))
      // Save the distance calculation time
      this.logger.addMsg("DISTANCES CALCULATION TIME", "Elapsed time: %s".format(nanoTimeToString(distancesTime)))
      // Save the time
      this.logger.addMsg("TIME", "Elapsed time: %s".format(nanoTimeToString(finishTime - initTime)))
      // Save the log
      this.logger.storeFile(file.get + "_CNN")
    }

    this.data
  }
}
