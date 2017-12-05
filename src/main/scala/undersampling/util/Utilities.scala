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
}