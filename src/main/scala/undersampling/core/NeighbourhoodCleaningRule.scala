package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

/** Neighbourhood Cleaning Rule. Original paper: "Improving Identification of Difficult Small Classes by Balancing Class Distribution" by J. Laurikkala.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class NeighbourhoodCleaningRule(override private[undersampling] val data: Data,
                                override private[undersampling] val seed: Long = System.currentTimeMillis(),
                                override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** Compute the Neighbourhood Cleaning Rule (NCL)
    *
    * @param file      file to store the log. If its set to None, log process would not be done
    * @param distance  distance to use when calling the NNRule algorithm
    * @param k         number of neighbours to use when computing k-NN rule (normally 3 neighbours)
    * @param threshold consider a class to be undersampled if the number of instances of this class is
    *                  greater than data.size * threshold
    * @return Data structure with all the important information and index of elements kept
    */
  def sample(file: Option[String] = None, distance: Distances.Distance = Distances.EUCLIDEAN, k: Int = 3, threshold: Double = 0.5): Data = {
    // Note: the notation used to refers the subsets of data is the used in the original paper.

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

    val enn = new EditedNearestNeighbor(this.data)
    val resultENN: Data = enn.sample(file = None, distance = distance, k = k)
    val indexA1: Array[Int] = classesToWorkWith.indices.diff(resultENN._index.toList).toArray

    val minorityClassIndex: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (c, i) if c == this.untouchableClass => i }

    val (predictedLabels, neighbours): (Array[Any], Array[Array[Int]]) = minorityClassIndex.map { i: Int =>
      nnRule(distances = distances(i), selectedElements = minorityClassIndex.indices.diff(List(i)).toArray, labels = classesToWorkWith, k = k)
    }.unzip

    val neighbourhoodBooleanIndex: Array[Boolean] = (predictedLabels zip (minorityClassIndex map classesToWorkWith)).map((c: (Any, Any)) => c._1 != c._2)

    val selectedNeighbours: Array[Int] = (boolToIndex(neighbourhoodBooleanIndex) map neighbours).flatten.distinct

    val classesToUnderSample: Array[Any] = this.counter.collect { case (c, num_values) if c != this.untouchableClass &&
      num_values > dataToWorkWith.length * threshold => c
    }.toArray

    val indexA2: Array[AnyVal] = selectedNeighbours.map((i: Int) => if (classesToUnderSample.indexOf(classesToWorkWith(i)) != -1) i)

    val finalIndex: Array[Int] = classesToWorkWith.indices.diff((indexA1 ++ indexA2).toList).toArray

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
      this.logger.storeFile(file.get + "_NCL")
    }

    this.data
  }
}
