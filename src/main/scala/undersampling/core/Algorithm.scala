package undersampling.core

import undersampling.data.UndersamplingData
import undersampling.io.Logger
import undersampling.util.Utilities.mode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Base class to all the algorithms
  *
  * @param data data to work with
  * @param seed seed to use. If it is not provided, it will use the system time
  * @author Néstor Rodríguez Vico
  */
private[undersampling] class Algorithm(private[undersampling] val data: UndersamplingData, private[undersampling] val seed: Long = System.currentTimeMillis()) {
  private[undersampling] val y: Array[Any] = data._originalClasses
  // Logger object to log the execution of the algorithms
  private[undersampling] val logger = new Logger
  // Count the number of instances for each class
  private[undersampling] val counter: Array[(Any, Int)] = this.y.groupBy(identity).mapValues((_: Array[Any]).length).toArray.sortBy { case (_, d) => d }
  // In certain algorithms, reduce the minority class is forbidden, so let's detect what class is it if the user don't set one at pleasure
  private[undersampling] var untouchableClass: Any = this.counter.head._1
  // Extra information to obtain the Imbalanced Ratio
  private[undersampling] val minorityElements: Int = this.counter.head._2
  private[undersampling] val majorityElements: Int = this.counter.tail.map((_: (Any, Int))._2).sum
  // Remove NA values and change nominal values to numeric values
  private[undersampling] val x: Array[Array[Double]] = processData(data)
  // Info to normalize the data
  private[undersampling] val maxV: Array[Double] = this.x.transpose.map((col: Array[Double]) => col.max)
  private[undersampling] val minV: Array[Double] = this.x.transpose.map((col: Array[Double]) => col.min)
  // Normalize the data as follow: for each column, x, (x-min(x))/(max(x)-min(x))
  private[undersampling] val normalizedData: Array[Array[Double]] = (this.x.transpose zip (this.minV zip this.maxV)).map((tuple: (Array[Double], (Double, Double))) =>
    tuple._1.map((element: Double) => (element - tuple._2._1).toFloat / (tuple._2._2 - tuple._2._1))).transpose
  // Shuffle the data to make it random
  private[undersampling] val random: Random = new util.Random(seed)
  private[undersampling] val index: List[Int] = this.random.shuffle(this.y.indices.toList)
  private[undersampling] val randomizedX: Array[Array[Double]] = (this.index map this.normalizedData).toArray
  private[undersampling] val randomizedY: Array[Any] = (this.index map this.y).toArray

  /** Convert a data object into a matrix of doubles, taking care of missing values and nominal columns.
    * Missing data was treated using the most frequent value for nominal variables and the median for numeric variables.
    * Nominal columns are converted to doubles.
    *
    * @param data data to process
    * @return matrix of doubles containing the data.
    */
  def processData(data: UndersamplingData): Array[Array[Double]] = {
    val processedData: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]](0)

    for (column <- data._originalData.transpose.zipWithIndex) {
      // let's look for the NA values
      val naIndex: Array[Int] = column._1.zipWithIndex.filter((_: (Any, Int))._1 == "undersampling_NA").map((_: (Any, Int))._2)
      // If they exist
      if (naIndex.length != 0) {
        // Take the index of the elements that are not NA
        val nonNAIndex: Array[Int] = column._1.zipWithIndex.filter((_: (Any, Int))._1 != "undersampling_NA").map((_: (Any, Int))._2)
        // If the column is a nominal value
        if (!data._nominal.contains(column._2)) {
          // compute the mean of the present values
          val arrayDouble: Array[Double] = (nonNAIndex map column._1).map((_: Any).asInstanceOf[Double])
          val mean: Double = arrayDouble.sum / arrayDouble.length
          val array: Array[Any] = column._1
          // replace all the NA values with the mean
          for (index <- naIndex)
            array(index) = mean

          processedData += array.map((_: Any).asInstanceOf[Double])
        } else {
          val m: Any = mode(nonNAIndex map column._1)
          val array: Array[Any] = column._1
          // replace all the NA values with the mode
          for (index <- naIndex)
            array(index) = m

          // After replacing the NA values, we change them to numerical values (0, 1, 2, ..., N)
          val uniqueValues: Array[Any] = array.distinct
          val dict: mutable.Map[Any, Double] = collection.mutable.Map[Any, Double]()
          var counter: Double = 0.0
          for (value <- uniqueValues) {
            dict += (value -> counter)
            counter += 1.0
          }

          for (i <- array.indices) {
            array(i) = dict(array(i))
          }

          processedData += array.map((_: Any).asInstanceOf[Double])
        }
      } else {
        // If there is no NA values, copy the column as it is
        val array: Array[Double] = column._1.map((_: Any).asInstanceOf[Double])
        processedData += array
      }
    }

    processedData.toArray.transpose
  }

  /** Change the untouchable class. Needed for som algorithms
    *
    * @param c desired class to be the untouchable one
    */
  def setUntouchableClass(c: Any): Unit = this.untouchableClass = c

  /** Compute the imbalanced ratio (number of instances of all the classes except the minority one divided by number of
    * instances of the minority class)
    *
    * @param counter Array containing a pair representing: (class, number of elements)
    * @return the imbalanced ratio
    */
  def imbalancedRatio(counter: Array[(Any, Int)]): Float = {
    (counter.map((_: (Any, Int))._2).sum.toFloat - this.minorityElements) / this.minorityElements
  }

  /** Denormalize the data to return the data untouched
    *
    * @param x data to work with
    * @return data denormalized
    */
  def denormalizedData(x: Array[Array[Double]]): Array[Array[Double]] = {
    (x.transpose zip (this.minV zip this.maxV)).map((tuple: (Array[Double], (Double, Double))) => tuple._1.map((element: Double) =>
      tuple._2._1 + element * (tuple._2._2 - tuple._2._1))).transpose
  }

  /** Convert a double matrix to a matrix of Any
    *
    * @param d data to be converted
    * @return matrix of Any
    */
  def toXData(d: Array[Array[Double]]): Array[Array[Any]] = {
    val r: Array[Array[Any]] = new Array[Array[Any]](d.length)
    for (row <- d.zipWithIndex) {
      r(row._2) = row._1.map((_: Double).asInstanceOf[Any])
    }

    r.asInstanceOf[Array[Array[Any]]]
  }
}
