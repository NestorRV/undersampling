package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.util.Random

/** NearMiss. Original paper: "kNN Approach to Unbalanced Data Distribution: A Case Study involving Information
  * Extraction" by Jianping Zhang and Inderjeet Mani.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class NearMiss(private[undersampling] val data: Data,
               override private[undersampling] val seed: Long = System.currentTimeMillis(),
               override private[undersampling] val minorityClass: Any = -1) extends Algorithm {


  /** Compute NearMiss undersampling (NM rule)
    *
    * @param file        file to store the log. If its set to None, log process would not be done
    * @param distance    distance to use when calling the NNRule algorithm
    * @param version     version of the algorithm to execute
    * @param nNeighbours number of neighbours to take for each minority example (only used if version is set to 3)
    * @param ratio       ratio to know how many majority class examples to preserve. By default it's set to 1 so there
    *                    will be the same minority class examples as majority class examples. If it's set to 2, there
    *                    will be the twice as many majority class examples as minority class examples
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance = Distances.EUCLIDEAN, version: Int = 1, nNeighbours: Int = 3, ratio: Double = 1.0): Data = {
    // Use normalized data for EUCLIDEAN distance and randomized data
    val dataToWorkWith: Array[Array[Double]] = if (distance == Distances.EUCLIDEAN)
      (this.index map zeroOneNormalization(this.data)).toArray else
      (this.index map this.x).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Start the time
    val initTime: Long = System.nanoTime()

    val initDistancesTime: Long = System.nanoTime()
    // Distances among the elements
    val distances: Array[Array[Double]] = computeDistances(dataToWorkWith, distance, this.data._nominal, this.y)
    val distancesTime: Long = System.nanoTime() - initDistancesTime

    val majElements: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (label, i) if label != this.untouchableClass => i }
    val minElements: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (label, i) if label == this.untouchableClass => i }

    val selectedMajElements: Array[Int] = if (version == 1) {
      majElements.map { instance: Int =>
        val result: (Any, Array[Int]) = nnRule(distances = distances(instance), selectedElements = minElements, labels = classesToWorkWith, k = 3)
        (instance, (result._2 map distances(instance)).sum / result._2.length)
      }.sortBy((_: (Int, Double))._2).map((_: (Int, Double))._1)
    } else if (version == 2) {
      majElements.map { instance: Int =>
        val result: (Any, Array[Int]) = nnRule(distances = distances(instance), selectedElements = minElements, labels = classesToWorkWith, k = 3, which = "farthest")
        (instance, (result._2 map distances(instance)).sum / result._2.length)
      }.sortBy((_: (Int, Double))._2).map((_: (Int, Double))._1)
    } else if (version == 3) {
      // We shuffle the data because, at last, we are going to take, at least, minElements.length * ratio elements and if
      // we don't shuffle, we only take majority elements examples that are near to the first minority class examples
      new Random(this.seed).shuffle(minElements.flatMap { instance: Int =>
        nnRule(distances = distances(instance), selectedElements = majElements,
          labels = classesToWorkWith, k = nNeighbours)._2
      }.distinct.toList).toArray
    } else {
      throw new Exception("Invalid argument: version should be: 1, 2 or 3")
    }

    val finalIndex: Array[Int] = minElements ++ selectedMajElements.take((minElements.length * ratio).toInt)

    // Stop the time
    val finishTime: Long = System.nanoTime()

    this.data._resultData = (finalIndex map this.index).sorted map this.data._originalData
    this.data._resultClasses = (finalIndex map this.index).sorted map this.data._originalClasses
    this.data._index = (finalIndex map this.index).sorted

    if (file.isDefined) {
      // Recount of classes
      val newCounter: Map[Any, Int] = (finalIndex map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length)

      this.logger.addMsg("ORIGINAL SIZE: %d".format(dataToWorkWith.length))
      this.logger.addMsg("NEW DATA SIZE: %d".format(finalIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE: %s".format(100 - (finalIndex.length.toFloat / dataToWorkWith.length) * 100))

      this.logger.addMsg("ORIGINAL IMBALANCED RATIO: %s".format(imbalancedRatio(this.counter, this.untouchableClass)))
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO: %s".format(imbalancedRatio(newCounter, this.untouchableClass)))

      // Save the distance calculation time
      this.logger.addMsg("DISTANCES CALCULATION TIME: %s".format(nanoTimeToString(distancesTime)))
      // Save the time
      this.logger.addMsg("TOTAL ELAPSED TIME: %s".format(nanoTimeToString(finishTime - initTime)))

      // Save the log
      this.logger.storeFile(file.get + "_NM")
    }

    this.data
  }
}
