package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.util.Random

/** Easy Ensemble algorithm. Original paper: "Exploratory Undersampling for Class-Imbalance Learning" by Xu-Ying Liu, Jianxin Wu and Zhi-Hua Zhou.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class EasyEnsemble(override private[undersampling] val data: Data,
                   override private[undersampling] val seed: Long = System.currentTimeMillis(),
                   override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** This algorithm preserve, at least, numberOfElements elements from the majority class
    *
    * @param file        file to store the log. If its set to None, log process would not be done
    * @param ratio       ratio to know how many majority class examples to preserve. By default it's set to 1 so there
    *                    will be the same minority class examples as majority class examples. If it's set to 2, there
    *                    will be the twice as many majority class examples as minority class examples
    * @param replacement whether or not to sample randomly with replacement or not. false by default
    * @param n_times     times to perform the random undersampling
    * @return Data structure with all the important information and index of elements kept
    */
  def sample(file: Option[String] = None, ratio: Double = 1.0, replacement: Boolean = false, n_times: Int): Data = {
    // Use randomized data 
    val dataToWorkWith: Array[Array[Double]] = (this.index map this.x).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Start the time
    val initTime: Long = System.nanoTime()

    // The elements in the minority class (untouchableClass) are maintained
    val minorityIndex: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (label, i) if label == this.untouchableClass => i }
    // Get the index of the elements in the majority class
    val random: Random = new Random(this.seed)

    val majIndex: List[Int] = classesToWorkWith.zipWithIndex.collect { case (label, i) if label != this.untouchableClass => i }.toList
    val majElements: Array[Int] = (0 until n_times).flatMap { _: Int =>
      val majorityIndex: Array[Int] = random.shuffle(majIndex).toArray
      if (!replacement) majorityIndex.slice(0, (minorityIndex.length * ratio).toInt) else majorityIndex.indices.map((_: Int) =>
        random.nextInt(majorityIndex.length)).toArray map majorityIndex
    }.toArray

    // Make an histogram and select the majority class examples that have been selected more times
    val majorityIndexHistogram: Array[(Int, Int)] = majElements.groupBy(identity).mapValues((_: Array[Int]).length).toArray.sortBy((_: (Int, Int))._2).reverse

    val majorityIndex: Array[Int] = majorityIndexHistogram.slice(0, (minorityIndex.length * ratio).toInt).map((_: (Int, Int))._1)

    // Get the index of the reduced data
    val finalIndex: Array[Int] = minorityIndex ++ majorityIndex

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

      // Save the time
      this.logger.addMsg("TOTAL ELAPSED TIME: %s".format(nanoTimeToString(finishTime - initTime)))

      // Save the log
      this.logger.storeFile(file.get + "_EE")
    }

    this.data
  }
}