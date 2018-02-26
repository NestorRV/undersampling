package undersampling.util

import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.{pow, sqrt}

/** Set of utilities functions
  *
  * @author Néstor Rodríguez Vico
  */
object Utilities {

  /** Enumeration to store the possible distances
    *
    * EUCLIDEAN_NOMINAL: Euclidean Distance for numeric values plus Nominal Distance (minimum distance, 0,
    * if the two elements are equal, maximum distance, 1, otherwise) for nominal values.
    *
    * HVDM: Proposed in "Improved Heterogeneous Distance Functions" by "D. Randall Wilson and Tony R. Martinez"
    *
    */
  object Distances extends Enumeration {
    type Distance = Value
    val EUCLIDEAN_NOMINAL: Distances.Value = Value
    val HVDM: Distances.Value = Value
  }

  /** Compute the mode of an array
    *
    * @param data array to compute the mode
    * @return the mode of the array
    */
  def mode(data: Array[Any]): Any = {
    val uniqueValues: Array[Any] = data.distinct
    val dict: mutable.Map[Any, Int] = collection.mutable.Map[Any, Int]()
    for (value <- uniqueValues) {
      dict += (value -> 0)
    }

    for (e <- data) {
      dict(e) += 1
    }

    dict.toSeq.sortBy((_: (Any, Int))._2).reverse.head._1
  }

  /** Compute the Euclidean Distance between two points
    *
    * @param xs first element
    * @param ys second element
    * @return Euclidean Distance between xs and ys
    */
  def euclideanDistance(xs: Array[Double], ys: Array[Double]): Double = {
    sqrt((xs zip ys).map((pair: (Double, Double)) => pow(pair._2 - pair._1, 2)).sum)
  }

  /** Compute the Euclidean-Nominal Distance between two points
    *
    * @param xs            first element
    * @param ys            second element
    * @param nominalValues array indicating the index of the nominal values
    * @return Euclidean-Nominal Distance between xs and ys
    */
  def euclideanNominalDistance(xs: Array[Double], ys: Array[Double], nominalValues: Array[Int]): Double = {
    sqrt((xs zip ys).zipWithIndex.map((element: ((Double, Double), Int)) =>
      if (nominalValues.contains(element._2)) if (element._1._2 == element._1._1) 0 else 1 else pow(element._1._2 - element._1._1, 2)).sum)
  }

  def hvdm(xs: Array[Double], ys: Array[Double]): Double = {
    0.0
  }

  /** Compute the distances all elements against all elements
    *
    * @param data     elements to compute the distance
    * @param distance distance to use
    * @param nominal  array indicating the nominale elements, if present
    * @return matrix array with the distances
    */
  def computeDistances(data: Array[Array[Double]], distance: Distances.Distance, nominal: Array[Int]): Array[Array[Double]] = {
    val distances: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]](0)
    for (i <- data.indices) {
      val row = new Array[Double](data.length)
      for (j <- i until row.length) {
        if (distance == Distances.HVDM)
          row(j) = hvdm(data(i), data(j))
        else if (distance == Distances.EUCLIDEAN_NOMINAL && nominal.length == 0)
          row(j) = euclideanDistance(data(i), data(j))
        else
          row(j) = euclideanNominalDistance(data(i), data(j), nominal)
      }
      distances += row
    }

    for (i <- data.indices) {
      for (j <- i until data.length) {
        distances(j)(i) = distances(i)(j)
      }
    }

    distances.toArray
  }

  /** Decide the label using the NNRule considering k neighbors of data set
    *
    * @param distances        distances between the newInstance element and the rest of elements
    * @param selectedElements elements to consider
    * @param labels           labels associated to each point in data
    * @param k                number of neighbors to consider
    * @return the label associated to newPoint and the index of the k-nearest neighbours, else None
    */
  def nnRule(distances: Array[Double], selectedElements: Array[Int], labels: Array[Any], k: Int): (Any, Array[Int]) = {
    val neighbours: Array[(Double, Int)] = (selectedElements map distances.zipWithIndex).sortBy { case (d, _) => d }
    val kBestNeighbours: Array[(Double, Int)] = neighbours.slice(0, if (k > selectedElements.length) selectedElements.length else k)
    val index: Array[Int] = kBestNeighbours.map((d: (Double, Int)) => d._2)
    (mode(index map labels), index)
  }

  /** Convert nanoseconds to minutes, seconds and milliseconds
    *
    * @param elapsedTime nanoseconds to be converted
    * @return String representing the conversion
    */
  def nanoTimeToString(elapsedTime: Long): String = {
    val minutes: Long = TimeUnit.NANOSECONDS.toMinutes(elapsedTime)
    val seconds: Long = TimeUnit.NANOSECONDS.toSeconds(elapsedTime) - TimeUnit.MINUTES.toSeconds(minutes)
    val millis: Long = TimeUnit.NANOSECONDS.toMillis(elapsedTime) - TimeUnit.MINUTES.toMillis(minutes) - TimeUnit.SECONDS.toMillis(seconds)
    "%03d min, %03d sec %03d millis".format(minutes, seconds, millis)
  }
}