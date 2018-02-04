package undersampling.core

import undersampling.data.UndersamplingData

/** Compute a random undersampling.
  *
  * @param data data to work with
  * @param seed seed to use. If it is not provided, it will use the system time
  * @author Néstor Rodríguez Vico
  */
class RandomUndersampling(override private[undersampling] val data: UndersamplingData,
                          override private[undersampling] val seed: Long = System.currentTimeMillis()) extends Algorithm(data, seed) {

  /** This algorithm preserve, at least, numberOfElements elements from the majority class
    *
    * @param file             file to store the log. If its set to None, log process would not be done
    * @param numberOfElements number of elements to preserve from the majority class
    * @return UndersamplingData structure with all the important information and index of elements kept
    */
  def sample(file: Option[String] = None, numberOfElements: Int): (UndersamplingData, Array[Int]) = {
    // The elements in the minority class (untouchableClass) are maintained
    val minorityIndex: Array[Int] = this.randomizedY.zipWithIndex.collect { case (label, i) if label == this.untouchableClass => i }
    // It is not possible to select more elements than the available
    val elementsToSelect: Int = if (numberOfElements > this.majorityElements) this.majorityElements else numberOfElements
    // Get the index of the elements in the majority class
    val majorityIndex: Array[Int] = random.shuffle(this.randomizedY.zipWithIndex.collect { case (label, i) if label != this.untouchableClass => i }.toList).toArray.slice(0, elementsToSelect)
    // Get the index of the reduced data
    val finalIndex: Array[Int] = minorityIndex ++ majorityIndex

    if (file.isDefined) {
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(this.normalizedData.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))
      // Recount of classes
      val newCounter: Array[(Any, Int)] = (finalIndex map this.randomizedY).groupBy(identity).mapValues((_: Array[Any]).length).toArray
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(finalIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (finalIndex.length.toFloat / this.randomizedX.length) * 100).toString)
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO", "NEW: %s".format(imbalancedRatio(newCounter)))
      // Save the log
      this.logger.storeFile(file.get + "_RU")
    }

    this.data._resultData = (finalIndex map this.index).sorted map this.data._originalData
    this.data._resultClasses = (finalIndex map this.index).sorted map this.data._originalClasses

    (this.data, (finalIndex map this.index).sorted)
  }
}