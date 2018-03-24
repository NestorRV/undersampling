package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.collection.mutable.ArrayBuffer
import scala.math.min

/** Class Purity Maximization algorithm. Original paper: "An Unsupervised Learning Approach to Resolving the
  * Data Imbalanced Issue in Supervised Learning Problems in Functional Genomics" by Kihoon Yoon and Stephen Kwek.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class ClassPurityMaximization(override private[undersampling] val data: Data,
                              override private[undersampling] val seed: Long = System.currentTimeMillis(),
                              override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  private[undersampling] val centers: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
  private[undersampling] var distances: Array[Array[Double]] = _

  /** Undersampling method based in ClassPurityMaximization clustering
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance = Distances.EUCLIDEAN): Data = {
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
    this.distances = computeDistances(dataToWorkWith, distance, this.data._nominal, this.y)
    val distancesTime: Long = System.nanoTime() - initDistancesTime

    // Count the number of positive and negative elements
    val posElements: Int = this.counter.head._2
    val negElements: Int = this.counter.tail.values.sum
    // Compute the impurity
    val impurity: Double = posElements.asInstanceOf[Double] / negElements.asInstanceOf[Double]
    // The first cluster contains all the elements
    val cluster: Array[Int] = new Array[Int](dataToWorkWith.length).indices.toArray

    purityMaximization(impurity, cluster, 0)

    // Stop the time
    val finishTime: Long = System.nanoTime()

    // Save the data
    this.data._resultData = (this.centers.toArray map this.index).sorted map this.data._originalData
    this.data._resultClasses = (this.centers.toArray map this.index).sorted map this.data._originalClasses
    this.data._index = (this.centers.toArray map this.index).sorted

    if (file.isDefined) {
      // Recount of classes
      val newCounter: Map[Any, Int] = (this.centers.toArray map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length)

      this.logger.addMsg("ORIGINAL SIZE: %d".format(dataToWorkWith.length))
      this.logger.addMsg("NEW DATA SIZE: %d".format(this.centers.toArray.length))
      this.logger.addMsg("REDUCTION PERCENTAGE: %s".format(100 - (this.centers.toArray.length.toFloat / dataToWorkWith.length) * 100))

      this.logger.addMsg("ORIGINAL IMBALANCED RATIO: %s".format(imbalancedRatio(this.counter, this.untouchableClass)))
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO: %s".format(imbalancedRatio(newCounter, this.untouchableClass)))

      // Save the distance calculation time
      this.logger.addMsg("DISTANCES CALCULATION TIME: %s".format(nanoTimeToString(distancesTime)))
      // Save the time
      this.logger.addMsg("TOTAL ELAPSED TIME: %s".format(nanoTimeToString(finishTime - initTime)))

      // Save the log
      this.logger.storeFile(file.get + "_CPM")
    }
    this.data
  }

  /** Purity maximization method
    *
    * @param parentImpurity impurity of the parent cluster
    * @param parentCluster  elements in the parent cluster
    * @param center         center of the cluster
    */
  private[undersampling] def purityMaximization(parentImpurity: Double, parentCluster: Array[Int], center: Int): Unit = {
    val classes: Array[Any] = (this.index map this.y).toArray

    val cluster1: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
    val cluster2: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
    val posElements: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
    val negElements: ArrayBuffer[Int] = new ArrayBuffer[Int](0)

    var center1: Int = 0
    var center2: Int = 0
    var pointer: Int = 0
    var impurity: Double = Double.PositiveInfinity
    var impurity1: Double = Double.PositiveInfinity
    var impurity2: Double = Double.PositiveInfinity

    parentCluster.foreach((f: Int) => if (classes(f) == this.untouchableClass) posElements += f else negElements += f)

    val pairs: ArrayBuffer[(Int, Int)] = for {x <- negElements; y <- posElements} yield (x, y)

    while (parentImpurity <= impurity) {
      if (pointer >= pairs.length) {
        this.centers += center
        return
      }

      center1 = pairs(pointer)._1
      center2 = pairs(pointer)._2

      parentCluster.foreach((element: Int) => if (this.distances(element)(center1) < this.distances(element)(center2)) cluster1 += element else cluster2 += element)

      if (cluster1.nonEmpty)
        impurity1 = cluster1.count((element: Int) => classes(element) == this.untouchableClass).toDouble / cluster1.length
      else {
        this.centers += center2
        return
      }

      if (cluster2.nonEmpty)
        impurity2 = cluster2.count((element: Int) => classes(element) == this.untouchableClass).toDouble / cluster2.length
      else {
        this.centers += center1
        return
      }

      impurity = min(impurity1, impurity2)
      pointer += 1
    }

    purityMaximization(impurity1, cluster1.toArray, center1)
    purityMaximization(impurity2, cluster2.toArray, center2)
  }
}
