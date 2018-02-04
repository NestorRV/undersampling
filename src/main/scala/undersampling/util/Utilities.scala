package undersampling.util

import scala.collection.mutable
import scala.math.{pow, sqrt}

/** Set of utilities functions
  *
  * @author Néstor Rodríguez Vico
  */
object Utilities {

  /** Enumeration to store the possible distances
    * SAMELABELS: minimum distance, 0, if the two elements has has the same labels, maximum distance, 1, otherwise
    * EUCLIDEAN: Euclidean Distance
    */
  object Distances extends Enumeration {
    type Distance = Value
    val SAMELABELS, EUCLIDEAN: Distances.Value = Value
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

  /** Compute the sameLabelsDistance. If two elements has the same label,
    * the distance is minimum (0), otherwise it is maximum, 1
    *
    * @param firstLabel  label of the first element
    * @param secondLabel label of the second element
    * @return sameLabelsDistance between two points
    */
  def sameLabelsDistance(firstLabel: Any, secondLabel: Any): Int = {
    if (firstLabel == secondLabel) 0 else 1
  }

  /** Compute the Euclidean Distance between two points
    *
    * @param xs first element
    * @param ys second element
    * @return Euclidean Distance between xs and ys
    */
  def euclideanDistance(xs: Array[Double], ys: Array[Double]): Double = {
    sqrt((xs zip ys).map { case (x, y) => pow(y - x, 2) }.sum)
  }

  /** Decide the label of newInstance using the NNRule considering k neighbors of data set
    *
    * @param data             data where to search for
    * @param labels           labels associated to each point in data
    * @param newInstance      the point you want to classify
    * @param newInstanceLabel label of the new point to classify
    * @param k                number of neighbors to consider
    * @param distance         distance to use. Available in enumeration Distances. SAMELABELS by default
    * @param getIndex         if getIndex is true, the index of the k-nearest neighbours will be returned, None is returned otherwise
    * @return the label associated to newPoint and, if getIndex is true, the index of the k-nearest neighbours, else None
    */
  def nnRule(data: Array[Array[Double]], labels: Array[Any], newInstance: Array[Double], newInstanceLabel: Any, k: Int,
             distance: Distances.Distance = Distances.SAMELABELS, getIndex: Boolean = false): (Any, Option[Array[Int]]) = {
    if (distance == Distances.SAMELABELS) {
      val distances: Array[(Int, Int)] = labels.map((x: Any) => sameLabelsDistance(x, newInstanceLabel)).zipWithIndex.sortBy { case (d, _) => d }
      val bestDistances: Array[(Int, Int)] = distances.slice(0, if (k > data.length) data.length else k)
      val index: Array[Int] = bestDistances.map((d: (Int, Int)) => d._2)
      if (getIndex) {
        (mode(index map labels), Option(index))
      } else {
        (mode(index map labels), None)
      }
    } else if (distance == Distances.EUCLIDEAN) {
      val distances: Array[(Double, Int)] = data.map((x: Array[Double]) => euclideanDistance(x, newInstance)).zipWithIndex.sortBy { case (d, _) => d }
      val bestDistances: Array[(Double, Int)] = distances.slice(0, if (k > data.length) data.length else k)
      val index: Array[Int] = bestDistances.map((d: (Double, Int)) => d._2)
      if (getIndex) {
        (mode(index map labels), Option(index))
      } else {
        (mode(index map labels), None)
      }
    } else {
      throw new Exception("Incorrect parameter: distance")
    }
  }
}