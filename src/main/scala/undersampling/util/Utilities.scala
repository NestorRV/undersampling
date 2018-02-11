package undersampling.util

import java.util.concurrent.TimeUnit

import scala.collection.mutable
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
    */
  object Distances extends Enumeration {
    type Distance = Value
    val EUCLIDEAN_NOMINAL: Distances.Value = Value
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

  /** Decide the label of newInstance using the NNRule considering k neighbors of data set
    *
    * @param data          data where to search for
    * @param labels        labels associated to each point in data
    * @param newInstance   the point you want to classify
    * @param nominalValues array indicating the index of the nominal values
    * @param k             number of neighbors to consider
    * @param distance      distance to use. Available in Distances enumeration. EUCLIDEAN_NOMINAL by default
    * @param getIndex      if getIndex is true, the index of the k-nearest neighbours will be returned, None is returned otherwise
    * @return the label associated to newPoint and, if getIndex is true, the index of the k-nearest neighbours, else None
    */
  def nnRule(data: Array[Array[Double]], labels: Array[Any], newInstance: Array[Double], nominalValues: Array[Int], k: Int,
             distance: Distances.Distance = Distances.EUCLIDEAN_NOMINAL, getIndex: Boolean = false): (Any, Option[Array[Int]]) = {
    if (distance == Distances.EUCLIDEAN_NOMINAL) {
      if (nominalValues.length != 0) {
        // if there are nominal values
        val distances: Array[(Double, Int)] = data.map((x: Array[Double]) => euclideanNominalDistance(x, newInstance, nominalValues)).zipWithIndex.sortBy { case (d, _) => d }
        val bestDistances: Array[(Double, Int)] = distances.slice(0, if (k > data.length) data.length else k)
        val index: Array[Int] = bestDistances.map((d: (Double, Int)) => d._2)
        if (getIndex) {
          (mode(index map labels), Option(index))
        } else {
          (mode(index map labels), None)
        }
      } else {
        // if there are not nominal values
        val distances: Array[(Double, Int)] = data.map((x: Array[Double]) => euclideanDistance(x, newInstance)).zipWithIndex.sortBy { case (d, _) => d }
        val bestDistances: Array[(Double, Int)] = distances.slice(0, if (k > data.length) data.length else k)
        val index: Array[Int] = bestDistances.map((d: (Double, Int)) => d._2)
        if (getIndex) {
          (mode(index map labels), Option(index))
        } else {
          (mode(index map labels), None)
        }
      }
    } else {
      throw new Exception("Incorrect parameter: distance")
    }
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