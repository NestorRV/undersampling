package undersampling.core

import undersampling.io.Logger

/** Compute a random undersampling.
  *
  * @param x             data to work with
  * @param y             labels of the data associated with x
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass integer representing the class to keep it untouched
  * @author Néstor Rodríguez Vico
  */
class RandomUndersampling(override private[undersampling] val x: Array[Array[Double]],
                          override private[undersampling] val y: Array[Int],
                          override private[undersampling] val seed: Long = System.currentTimeMillis(),
                          minorityClass: Int = 0) extends Algorithm(x, y, seed, minorityClass) {

  /** This algorithm preserve, at least, numberOfElements elements from the majority class
    *
    * @param file             file to store the log. If its set to None, log process would not be done
    * @param numberOfElements number of elements to preserve from the majority class
    * @return reduced data, reduced labels, index of elements kept
    */
  def sample(file: Option[String] = None, numberOfElements: Int): (Array[Array[Double]], Array[Int], Array[Int]) = {
    // The elements in the minority class (untouchableClass) are maintained
    val minorityIndex: Array[Int] = this.randomizedY.zipWithIndex.collect { case (label, i) if label == this.untouchableClass => i }
    // It is not possible to select more elements than the available
    val elementsToSelect: Int = if (numberOfElements > this.majorityElements) this.majorityElements else numberOfElements
    // Get the index of the elements in the majority class
    val majorityIndex: Array[Int] = random.shuffle(this.randomizedY.zipWithIndex.collect { case (label, i) if label != this.untouchableClass => i }.toList).toArray.slice(0, elementsToSelect)
    // Get the index of the reduced data
    val finalIndex: Array[Int] = minorityIndex ++ majorityIndex

    if (file.isDefined) {
      val logger: Logger = new Logger(numberLogs = 2)
      logger.info += "DATA SIZE REDUCTION INFORMATION. \nORIGINAL DATA SIZE: %s".format(this.normalizedData.length.toString)
      logger.info += "ORIGINAL IMBALANCED RATIO: %s".format(imbalancedRatio(this.counter))
      // Recount of classes
      val newCounter: Array[(Int, Int)] = (finalIndex map this.randomizedY).groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray
      logger.info(0) += "\nNEW DATA SIZE: %d\n".format(finalIndex.length)
      logger.info(0) += "\nREDUCTION PERCENTAGE: %f\n".format(100 - (finalIndex.length.toFloat / this.randomizedX.length) * 100)
      // Recompute the Imbalanced Ratio
      logger.addMsg("NEW IMBALANCED RATIO: %s".format(imbalancedRatio(newCounter)), 1)
      // Save the logs
      logger.storeFile(file + "_RU")
    }

    (finalIndex map this.randomizedX, finalIndex map this.randomizedY, finalIndex)
  }
}