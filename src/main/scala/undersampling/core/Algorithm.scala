package undersampling.core

import scala.util.Random

class Algorithm(private[undersampling] val x: Array[Array[Double]], private[undersampling] val y: Array[Int],
                private[undersampling] val seed: Long = System.currentTimeMillis(), minorityClass: Int = 0) {

  private[undersampling] val random: Random = new util.Random(seed)
  // Shuffle the data to make it random
  private[undersampling] val index: List[Int] = this.random.shuffle(y.indices.toList)
  private[undersampling] val randomizedX: Array[Array[Double]] = (this.index map this.x).toArray
  private[undersampling] val randomizedY: Array[Int] = (this.index map this.y).toArray
  // Count the number of instances for each class
  private[undersampling] val counter: Array[(Int, Int)] = this.randomizedY.groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray.sortBy { case (_, d) => d }
  // In certain algorithms, reduce the minority class is forbidden, so let's detect what class is it if the user don't
  // set one at pleasure
  private[undersampling] val untouchableClass: Int = if (minorityClass == 0) this.counter.head._1 else minorityClass
  // Extra information to obtain the Imbalanced Ratio
  private[undersampling] val minorityElements: Int = this.counter.head._2
  private[undersampling] val majorityElements: Int = this.counter.tail.map((_: (Int, Int))._2).sum
  // Info to normalize the data
  private[undersampling] val maxV: Array[Double] = this.randomizedX.transpose.map((x: Array[Double]) => x.max)
  private[undersampling] val minV: Array[Double] = this.randomizedX.transpose.map((x: Array[Double]) => x.min)
  // Normalize the data as follow: for each column, x, (x-min(x))/(max(x)-min(x))
  private[undersampling] val normalizedData: Array[Array[Double]] = (this.randomizedX.transpose zip (minV zip maxV)).map((row: (Array[Double], (Double, Double))) =>
    row._1.map((e: Double) => e - row._2._1 / (row._2._2 / row._2._1))).transpose
}
