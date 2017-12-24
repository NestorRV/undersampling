package undersampling.util

import smile.data.AttributeDataset
import scala.math.{pow, sqrt}

/** Set of utilities functions
  *
  * @author Néstor Rodríguez Vico
  */
object Utilities {

  /** Enumeration to store the possible distances
    * EUCLIDEAN: Euclidean Distance
    */
  object Distances extends Enumeration {
    type Distance = Value
    val EUCLIDEAN: Distances.Value = Value
  }

  /** Compute the mode of an array
    *
    * @param data array to compute the mode
    * @return the mode of the array
    */
  def mode(data: Array[Int]): Int = {
    data.groupBy((i: Int) => i).mapValues((_: Array[Int]).length).maxBy((_: (Int, Int))._2)._1
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

  /** Compute the selected distance
    *
    * @param xs       first element
    * @param ys       first element
    * @param distance distance to use. Available in enumeration Distances
    * @return distance between xs and ys
    */
  def computeDistance(xs: Array[Double], ys: Array[Double], distance: Distances.Distance): Double = {
    euclideanDistance(xs, ys)
  }

  /** Build a AttributeDataset with the data provided in x and the classes provided in y
    *
    * @param originalData original AttributeDataset to read the meta info
    * @param x            data to store in the AttributeDataset
    * @param y            labels associated to x
    * @return the final AttributeDataset
    */
  def toDataSet(originalData: AttributeDataset, x: Array[Array[Double]], y: Array[Int]): AttributeDataset = {
    val dataSet: AttributeDataset = new AttributeDataset(originalData.getName, originalData.attributes(), originalData.response().attribute())
    (x zip y).foreach((pair: (Array[Double], Int)) => dataSet.add(pair._1, pair._2))
    dataSet
  }

  /** Decide the label of newInstance using the NNRule considering k neighbors of data set
    *
    * @param data        data where to search for
    * @param labels      labels associated to each point in data
    * @param newInstance the point you want to classify
    * @param k           number of neighbors to consider
    * @param distance    distance to use. Available in enumeration Distances. Euclidean Distance by default
    * @return the label associated to newPoint
    */
  def nnRule(data: Array[Array[Double]], labels: Array[Int], newInstance: Array[Double], k: Int, distance: Distances.Distance = Distances.EUCLIDEAN): Int = {
    val distances: Array[(Double, Int)] = data.map((x: Array[Double]) => computeDistance(x, newInstance, distance)).zipWithIndex.sortBy { case (d, _) => d }
    val bestDistances: Array[(Double, Int)] = distances.slice(0, if (k > data.length) data.length else k)
    val index: Array[Int] = bestDistances.map((d: (Double, Int)) => d._2)
    mode(index map labels)
  }
}