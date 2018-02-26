package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** One-Side Selection algorithm. Original paper: "Addressing he Curse of Imbalanced
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
    * @param ratio    indicates the instances of the Tomek Links that are going to be remove. "all" will remove all instances,
    *                 "minority" will remove instances of the minority class and "not minority" will remove all the instances
    *                 except the ones of the minority class.
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance, ratio: String = "not minority"): Data = {
    // Note: the notation used to refers the subsets of data is the used in the original paper.

    // Use normalized data, but HVDM uses a special normalization
    // Use randomized data 
    val dataToWorkWith: Array[Array[Double]] = if (distance == Distances.HVDM)
      (this.index map this.hvdmNormalization(this.x)).toArray else
      (this.index map this.zeroOneNormalization(this.x)).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Start the time
    val initTime: Long = System.nanoTime()

    // Distances among the elements
    val distances: Array[Array[Double]] = computeDistances(dataToWorkWith, distance, this.data._nominal)

    // Let's save all the positive instances
    val positives: Array[Int] = classesToWorkWith.zipWithIndex.filter((pair: (Any, Int)) => pair._1 == this.untouchableClass).map((_: (Any, Int))._2)
    // Choose a random negative one
    val randomElement: Int = classesToWorkWith.indices.diff(positives)(Random.nextInt(classesToWorkWith.indices.diff(positives).size))
    // c is the union of positives with the random element
    val c: Array[Int] = positives ++ Array(randomElement)

    val labels: ArrayBuffer[Any] = new ArrayBuffer[Any]()
    // Let's classify S with the content of C
    for (index <- dataToWorkWith.indices) {
      labels += nnRule(distances = distances(index), selectedElements = c.diff(List(index)), labels = classesToWorkWith, k = 1)._1
    }
    // Look for the misclassified instances
    val misclassified: Array[Int] = (labels zip classesToWorkWith).zipWithIndex.filter((pair: ((Any, Any), Int)) => pair._1._1 != pair._1._2).map((_: ((Any, Any), Int))._2).toArray
    // Add the misclassified instances to C
    val finalC: Array[Int] = (misclassified ++ c).distinct

    // Construct a Data object to be passed to TomekLink
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
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE", "TIME"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(dataToWorkWith.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))

      // Recount of classes
      val newCounter: Array[(Any, Int)] = (finalIndex map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length).toArray
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(finalIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (finalIndex.length.toFloat / dataToWorkWith.length) * 100).toString)
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO", "NEW: %s".format(imbalancedRatio(newCounter)))
      // Save the time
      this.logger.addMsg("TIME", "Elapsed time: %s".format(nanoTimeToString(finishTime - initTime)))
      // Save the log
      this.logger.storeFile(file.get + "_OSS")
    }

    this.data
  }
}
