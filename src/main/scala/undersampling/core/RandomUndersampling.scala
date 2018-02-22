package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.util.Random

/** Compute a random undersampling.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class RandomUndersampling(override private[undersampling] val data: Data,
                          override private[undersampling] val seed: Long = System.currentTimeMillis(),
                          override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** This algorithm preserve, at least, numberOfElements elements from the majority class
    *
    * @param file             file to store the log. If its set to None, log process would not be done
    * @param numberOfElements number of elements to preserve from the majority class
    * @return Data structure with all the important information and index of elements kept
    */
  def sample(file: Option[String] = None, numberOfElements: Int): Data = {
    // Use randomized data 
    val dataToWorkWith: Array[Array[Double]] = (this.index map this.x).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray
    
    // Start the time
    val initTime: Long = System.nanoTime()

    // The elements in the minority class (untouchableClass) are maintained
    val minorityIndex: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (label, i) if label == this.untouchableClass => i }
    // It is not possible to select more elements than the available (all the elements except the ones associated with the untouchableClass)
    val elementsToSelect: Int = if (numberOfElements > this.counter.tail.map((_: (Any, Int))._2).sum) this.counter.tail.map((_: (Any, Int))._2).sum else numberOfElements
    // Get the index of the elements in the majority class
    val majorityIndex: Array[Int] = new Random(this.seed).shuffle(classesToWorkWith.zipWithIndex.collect { case (label, i) if label != this.untouchableClass => i }.toList).toArray.slice(0, elementsToSelect)
    // Get the index of the reduced data
    val finalIndex: Array[Int] = minorityIndex ++ majorityIndex

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
      this.logger.storeFile(file.get + "_RU")
    }

    this.data
  }
}