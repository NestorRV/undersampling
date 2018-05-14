package undersampling.core

import undersampling.data.Data
import undersampling.io.Logger

/** Base class to all the algorithms
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
private[undersampling] class Algorithm(private[undersampling] val data: Data, private[undersampling] val seed: Long = System.currentTimeMillis(),
                                       private[undersampling] val minorityClass: Any = -1) {
  // Remove NA values and change nominal values to numeric values
  private[undersampling] val x: Array[Array[Double]] = this.data._processedData
  private[undersampling] val y: Array[Any] = data._originalClasses
  // Logger object to log the execution of the algorithms
  private[undersampling] val logger: Logger = new Logger
  // Count the number of instances for each class
  private[undersampling] val counter: Map[Any, Int] = this.y.groupBy(identity).mapValues((_: Array[Any]).length)
  // In certain algorithms, reduce the minority class is forbidden, so let's detect what class is it if minorityClass is set to -1.
  // Otherwise, minorityClass will be used as the minority one
  private[undersampling] val untouchableClass: Any = if (this.minorityClass == -1) this.counter.minBy((c: (Any, Int)) => c._2)._1 else this.minorityClass
  // Index to shuffle (randomize) the data
  private[undersampling] val index: List[Int] = new util.Random(this.seed).shuffle(this.y.indices.toList)

}
