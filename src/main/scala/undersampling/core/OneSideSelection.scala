package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

/** One-Side Selection algorithm. Original paper: "Addressing the Curse of Imbalanced
  * Training Sets: One-Side Selection" by Miroslav Kubat and Stan Matwin.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class OneSideSelection(override private[undersampling] val data: Data,
                       override private[undersampling] val seed: Long = System.currentTimeMillis(),
                       override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** Compute the One-Side Selection algorithm.
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance): Data = {
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

    // Let's save all the positive instances
    val positives: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (label, i) if label == this.untouchableClass => i }
    // Choose a random negative one
    val randomElement: Int = classesToWorkWith.indices.diff(positives)(new util.Random(this.seed).nextInt(classesToWorkWith.length - positives.length))
    // c is the union of positives with the random element
    val c: Array[Int] = positives ++ Array(randomElement)

    // Let's classify S with the content of C
    val labels: Seq[(Int, Any)] = dataToWorkWith.indices.map { index: Int =>
      (index, nnRule(distances = distances(index), selectedElements = c.diff(List(index)), labels = classesToWorkWith, k = 1)._1)
    }

    // Look for the misclassified instances
    val misclassified: Array[Int] = labels.collect { case (i, label) if label != classesToWorkWith(i) => i }.toArray
    // Add the misclassified instances to C
    val finalC: Array[Int] = (misclassified ++ c).distinct

    // Construct a Data object to be passed to Tomek Link
    val auxData: Data = new Data(_nominal = this.data._nominal, _originalData = toXData(finalC map dataToWorkWith),
      _originalClasses = finalC map classesToWorkWith, _fileInfo = this.data._fileInfo)
    // But the untouchableClass must be the same
    val tl = new TomekLink(auxData, minorityClass = this.untouchableClass)
    val resultTL: Data = tl.sample(file = None, distance = distance)
    // The final index is the result of applying TomekLink to the content of C
    val finalIndex: Array[Int] = classesToWorkWith.indices.toList.diff(resultTL._index.toList map finalC).toArray

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
      this.logger.storeFile(file.get + "_OSS")
    }

    this.data
  }
}
