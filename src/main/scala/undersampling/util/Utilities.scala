package undersampling.util

import smile.data.AttributeDataset
import scala.math.{pow, sqrt}

/** Set of utilities functions
  *
  * @author Néstor Rodríguez Vico
  */
object Utilities {
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
    * @param xs first data
    * @param ys second data
    * @return Euclidean Distance between xs and ys
    */
  def euclideanDistance(xs: Array[Double], ys: Array[Double]): Double = {
    sqrt((xs zip ys).map { case (x, y) => pow(y - x, 2) }.sum)
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
    * @param labels      labels asociated to each point in data
    * @param newInstance the point you want to clasify
    * @param k           number of neighbors to consider
    * @return the label associated to newPoint
    */
  def nnRule(data: Array[Array[Double]], labels: Array[Int], newInstance: Array[Double], k: Int): Int = {
    val distances: Array[(Double, Int)] = data.map((x: Array[Double]) => euclideanDistance(x, newInstance)).zipWithIndex.sortBy { case (d, _) => d }
    val bestDistances: Array[(Double, Int)] = distances.slice(0, if (k > data.length) data.length else k)
    val index: Array[Int] = bestDistances.map((d: (Double, Int)) => d._2)
    mode(index map labels)
  }
}