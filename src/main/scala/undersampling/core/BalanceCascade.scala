package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Easy Ensemble algorithm. Original paper: "Exploratory Undersampling for Class-Imbalance Learning" by Xu-Ying Liu, Jianxin Wu and Zhi-Hua Zhou.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class BalanceCascade(private[undersampling] val data: Data,
                     override private[undersampling] val seed: Long = System.currentTimeMillis(),
                     override private[undersampling] val minorityClass: Any = -1) extends Algorithm {

  /** Compute the Balance Cascade algorithm.
    *
    * @param file        file to store the log. If its set to None, log process would not be done
    * @param distance    distance to use when calling the NNRule algorithm
    * @param k           number of neighbours to use when computing k-NN rule (normally 3 neighbours)
    * @param nMaxSubsets maximum number of subsets to generate
    * @param nFolds      number of subsets to create when applying cross-validation
    * @param ratio       ratio to know how many majority class examples to preserve. By default it's set to 1 so there
    *                    will be the same minority class examples as majority class examples. If it's set to 2, there
    *                    will be the twice as many majority class examples as minority class examples
    * @return array of Data structures with all the important information and index of elements kept for each subset
    */

  def sample(file: Option[String] = None, distance: Distances.Distance = Distances.EUCLIDEAN, k: Int = 3,
             nMaxSubsets: Int = 5, nFolds: Int = 5, ratio: Double = 1.0): Data = {
    // Use randomized data
    val dataToWorkWith: Array[Array[Double]] = (this.index map this.x).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Start the time
    val initTime: Long = System.nanoTime()

    val initDistancesTime: Long = System.nanoTime()
    // Distances among the elements
    val distances: Array[Array[Double]] = computeDistances(dataToWorkWith, distance, this.data._nominal, this.y)
    val distancesTime: Long = System.nanoTime() - initDistancesTime

    val random: Random = new Random(this.seed)

    var search: Boolean = true
    var subsetsCounter: Int = 0
    val mask: Array[Boolean] = Array.fill(classesToWorkWith.length)(true)
    val subsets: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]](0)

    while (search) {
      val indexToUnderSample: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
      val minorityIndex: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
      val classesCounter: Map[Any, Int] = (boolToIndex(mask) map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length)

      classesCounter.foreach { target: (Any, Int) =>
        val indexClass: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (c, i) if c == target._1 => i }
        if (target._1 != this.untouchableClass) {
          val sameClassBool: Array[Boolean] = mask.zipWithIndex.collect { case (c, i) if classesToWorkWith(i) == target._1 => c }
          val indexClassInterest: Array[Int] = boolToIndex(sameClassBool) map indexClass
          val indexTargetClass: List[Int] = random.shuffle((indexClassInterest map classesToWorkWith).indices.toList).take(this.counter(this.untouchableClass))
          indexToUnderSample ++= (indexTargetClass map indexClassInterest)
        } else {
          minorityIndex ++= indexClass
        }
      }

      subsetsCounter += 1
      val subset: Array[Int] = (indexToUnderSample ++ minorityIndex).toArray
      subsets += subset

      val classesToWorkWithSubset: Array[Any] = subset map classesToWorkWith
      val prediction: Array[Any] = kFoldPrediction(classesToWorkWithSubset, distances = distances, k = k, nFolds = nFolds).take(indexToUnderSample.length)

      val classifiedInstances: Array[Boolean] = ((indexToUnderSample.indices map classesToWorkWithSubset) zip prediction).map((e: (Any, Any)) => e._1 == e._2).toArray
      (boolToIndex(classifiedInstances) map indexToUnderSample).foreach((i: Int) => mask(i) = false)

      if (subsetsCounter == nMaxSubsets) search = false

      val finalTargetStats: Map[Any, Int] = (boolToIndex(mask) map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length)
      classesToWorkWith.distinct.filter((c: Any) => c != this.untouchableClass).foreach((c: Any) => if (finalTargetStats(c) < this.counter(this.untouchableClass)) search = false)
    }

    val minorityIndex: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (c, i) if c == this.untouchableClass => i }
    val majElements: Array[Int] = subsets.flatten.filter((e: Int) => classesToWorkWith(e) != this.minorityClass).toArray
    val majorityIndexHistogram: Array[(Int, Int)] = majElements.groupBy(identity).mapValues((_: Array[Int]).length).toArray.sortBy((_: (Int, Int))._2).reverse
    val majorityIndex: Array[Int] = majorityIndexHistogram.take((minorityIndex.length * ratio).toInt).map((_: (Int, Int))._1)

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