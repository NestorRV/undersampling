package undersampling.core

import undersampling.io.Logger

import scala.util.Random

/** Base class to all the algorithms
  *
  * @param x             data to work with
  * @param y             labels of the data associated with x
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass integer representing the class to keep it untouched. If it is set to 0, it will be computed
  * @author Néstor Rodríguez Vico
  */
private[undersampling] class Algorithm(private[undersampling] val x: Array[Array[Double]], private[undersampling] val y: Array[Int],
                                       private[undersampling] val seed: Long = System.currentTimeMillis(), minorityClass: Int = 0) {

  // Logger object to log the execution of the algorithms
  private[undersampling] val logger = new Logger
  // Shuffle the data to make it random
  private[undersampling] var random: Random = new util.Random(seed)
  private[undersampling] var index: List[Int] = this.random.shuffle(this.y.indices.toList)
  private[undersampling] var randomizedX: Array[Array[Double]] = (this.index map this.x).toArray
  private[undersampling] var randomizedY: Array[Int] = (this.index map this.y).toArray
  // Count the number of instances for each class
  private[undersampling] val counter: Array[(Int, Int)] = this.randomizedY.groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray.sortBy { case (_, d) => d }
  // In certain algorithms, reduce the minority class is forbidden, so let's detect what class is it if the user don't set one at pleasure
  private[undersampling] val untouchableClass: Int = if (minorityClass == 0) this.counter.head._1 else if (minorityClass > this.randomizedY.length) throw new Exception("minorityClass is too big") else minorityClass
  // Extra information to obtain the Imbalanced Ratio
  private[undersampling] val minorityElements: Int = this.counter.head._2
  private[undersampling] val majorityElements: Int = this.counter.tail.map((_: (Int, Int))._2).sum
  // Info to normalize the data
  private[undersampling] val maxV: Array[Double] = this.randomizedX.transpose.map((x: Array[Double]) => x.max)
  private[undersampling] val minV: Array[Double] = this.randomizedX.transpose.map((x: Array[Double]) => x.min)
  // Normalize the data as follow: for each column, x, (x-min(x))/(max(x)-min(x))
  private[undersampling] var normalizedData: Array[Array[Double]] = (this.randomizedX.transpose zip (this.minV zip this.maxV)).map((row: (Array[Double], (Double, Double))) =>
    row._1.map((e: Double) => e - row._2._1 / (row._2._2 / row._2._1))).transpose

  /** Shuffle the data
    *
    * @param seed seed to use in the Random object used to shuffle
    */
  def shuffle(seed: Long): Unit = {
    this.random = new util.Random(seed)
    this.index = this.random.shuffle(this.y.indices.toList)
    this.randomizedX = (this.index map this.x).toArray
    this.randomizedY = (this.index map this.y).toArray
    this.normalizedData = (this.randomizedX.transpose zip (this.minV zip this.maxV)).map((row: (Array[Double], (Double, Double))) =>
      row._1.map((e: Double) => e - row._2._1 / (row._2._2 / row._2._1))).transpose
  }

  /** Compute the imbalanced ratio (number of instances of all the classes except the minority one divided by number of
    * instances of the minority class)
    *
    * @param counter Array containing a pair representing: (class, number of elements)
    * @return the imbalanced ratio
    */
  def imbalancedRatio(counter: Array[(Int, Int)]): Float = {
    (counter.map((_: (Int, Int))._2).sum.toFloat - this.minorityElements) / this.minorityElements
  }
}
