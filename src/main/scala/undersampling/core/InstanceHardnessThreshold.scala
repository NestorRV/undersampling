package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._
import weka.classifiers.trees.J48
import weka.core.Instances

import scala.util.Random


/** Instance Hardness Threshold. Original paper: "An Empirical Study of Instance Hardness" by Michael R. Smith, Tony Martinez and Christophe Giraud-Carrier.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class InstanceHardnessThreshold(private[undersampling] val data: Data,
                                override private[undersampling] val seed: Long = System.currentTimeMillis(),
                                override private[undersampling] val minorityClass: Any = -1) extends Algorithm {

  /** Compute InstanceHardnessThreshold undersampling
    *
    * @param file   file to store the log. If its set to None, log process would not be done
    * @param nFolds number of subsets to create when applying cross-validation
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, nFolds: Int = 5): Data = {
    // Use randomized data
    val dataToWorkWith: Array[Array[Double]] = (this.index map this.x).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Start the time
    val initTime: Long = System.nanoTime()

    val random: Random = new Random(this.seed)
    // Each element is the index of test elements
    val indices: Array[Array[Int]] = random.shuffle(classesToWorkWith.indices.toList).toArray.grouped((classesToWorkWith.length.toFloat / nFolds).ceil.toInt).toArray
    val probabilities: Array[Double] = new Array[Double](classesToWorkWith.length)

    indices.foreach { testIndex: Array[Int] =>
      val trainIndex: Array[Int] = classesToWorkWith.indices.diff(testIndex).toArray

      val j48 = new J48
      j48.setOptions(Array("-U", "-M", "1"))

      val trainInstances: Instances = buildInstances(data = trainIndex map dataToWorkWith, classes = trainIndex map classesToWorkWith, fileInfo = this.data._fileInfo)
      val testInstances: Instances = buildInstances(data = testIndex map dataToWorkWith, classes = testIndex map classesToWorkWith, fileInfo = this.data._fileInfo)

      j48.buildClassifier(trainInstances)

      val probs: Array[Array[Double]] = testIndex.indices.map((i: Int) => j48.distributionForInstance(testInstances.instance(i))).toArray
      val classes: Array[Any] = (testIndex map classesToWorkWith).distinct
      val values: Array[Double] = (testIndex map classesToWorkWith).zipWithIndex.map((e: (Any, Int)) => probs(e._2)(classes.indexOf(e._1)))

      (testIndex zip values).foreach((i: (Int, Double)) => probabilities(i._1) = i._2)
    }

    val finalIndex: Array[Int] = classesToWorkWith.distinct.flatMap { targetClass: Any =>
      val indexTargetClass: Array[Int] = if (targetClass != this.untouchableClass) {
        val nSamples: Int = this.counter(this.untouchableClass)
        val targetIndex: Array[Int] = boolToIndex(classesToWorkWith.map((c: Any) => c == targetClass))
        val targetProbabilities: Array[Double] = targetIndex map probabilities
        val percentile: Double = (1.0 - (nSamples / this.counter(targetClass))) * 100.0
        val threshold: Double = targetProbabilities.sorted.apply(math.ceil((targetProbabilities.length - 1) * (percentile / 100.0)).toInt)
        boolToIndex((targetIndex map probabilities).map((e: Double) => e >= threshold))
      }
      else {
        classesToWorkWith.filter((c: Any) => c == targetClass).indices.toArray
      }

      indexTargetClass map boolToIndex(classesToWorkWith.map((c: Any) => c == targetClass))
    }

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
      this.logger.storeFile(file.get + "_IHTS")
    }

    this.data
  }
}
