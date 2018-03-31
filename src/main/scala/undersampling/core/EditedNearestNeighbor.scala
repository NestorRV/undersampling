package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.collection.parallel.mutable.ParArray

/** Edited Nearest Neighbour rule. Original paper: "Asymptotic Properties of Nearest Neighbor Rules Using Edited Data" by Dennis L. Wilson.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class EditedNearestNeighbor(override private[undersampling] val data: Data,
                            override private[undersampling] val seed: Long = System.currentTimeMillis(),
                            override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {


  /** Compute the Edited Nearest Neighbour rule (ENN rule)
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @param k        number of neighbours to use when computing k-NN rule (normally 3 neighbours)
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance = Distances.EUCLIDEAN, k: Int = 3): Data = {
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

    val indices: Array[Int] = classesToWorkWith.indices.toArray

    // indices.diff(List(index)) is to exclude the actual element -> LeaveOneOut
    val calculatedLabels: ParArray[(Int, (Any, Array[Int]))] = indices.par.map { index: Int =>
      (index, nnRule(distances = distances(index),
        selectedElements = indices.diff(List(index)), labels = classesToWorkWith, k = k))
    }
    // if the label matches (it is well classified) the element is useful
    val selectedElements: ParArray[Int] = calculatedLabels.par.collect { case (i, (label, _)) if label == classesToWorkWith(i) => i }

    // Stop the time
    val finishTime: Long = System.nanoTime()

    this.data._resultData = (selectedElements.toArray map this.index).sorted map this.data._originalData
    this.data._resultClasses = (selectedElements.toArray map this.index).sorted map this.data._originalClasses
    this.data._index = (selectedElements.toArray map this.index).sorted

    if (file.isDefined) {
      // Recount of classes
      val newCounter: Map[Any, Int] = (selectedElements.toArray map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length)

      this.logger.addMsg("ORIGINAL SIZE: %d".format(dataToWorkWith.length))
      this.logger.addMsg("NEW DATA SIZE: %d".format(selectedElements.length))
      this.logger.addMsg("REDUCTION PERCENTAGE: %s".format(100 - (selectedElements.length.toFloat / dataToWorkWith.length) * 100))

      this.logger.addMsg("ORIGINAL IMBALANCED RATIO: %s".format(imbalancedRatio(this.counter, this.untouchableClass)))
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO: %s".format(imbalancedRatio(newCounter, this.untouchableClass)))

      // Save the distance calculation time
      this.logger.addMsg("DISTANCES CALCULATION TIME: %s".format(nanoTimeToString(distancesTime)))
      // Save the time
      this.logger.addMsg("TOTAL ELAPSED TIME: %s".format(nanoTimeToString(finishTime - initTime)))

      // Save the log
      this.logger.storeFile(file.get + "_ENN")
    }

    this.data
  }
}
