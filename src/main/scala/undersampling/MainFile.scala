package undersampling

import smile.data.AttributeDataset
import undersampling.core._
import undersampling.io.{Reader, Writer}
import undersampling.util.Utilities._

/** An object to test the different algorithms
  *
  * @author Néstor Rodríguez Vico
  */
object MainFile {
  def main(args: Array[String]): Unit = {
    val data = List("contact-lenses.arff", "diabetes.arff", "iris.arff", "vote.arff", "weather.nominal.arff")
    val classes = List(4, 8, 4, 16, 4)

    for (dataset <- data zip classes) {
      println(dataset._1)
      val reader: Reader = new Reader(dataSet = "./data/" + dataset._1)
      val data: AttributeDataset = reader.readArff(classColumn = dataset._2)
      val x: Array[Array[Double]] = data.toArray(new Array[Array[Double]](data.size))
      val y: Array[Int] = data.toArray(new Array[Int](data.size))
      val writer: Writer = new Writer

      /*
      val ru = new RandomUndersampling(x, y)
      val result: (Array[Array[Double]], Array[Int], Array[Int]) = ru.sample(file = Option("./data/logs/" + dataset._1), numberOfElements = 10)
      val attributeDataset: AttributeDataset = writer.toDataSet(data, result._1, result._2)
      writer.writeArff(attributeDataset, "./data/results/" + dataset._1 + "_RU")
      */

      val cnn = new CondensedNearestNeighbor(x, y)
      val result: (Array[Array[Double]], Array[Int], Array[Int]) = cnn.sample(file = "./data/logs/" + dataset._1, distance = Distances.EUCLIDEAN)
      val attributeDataset: AttributeDataset = writer.toDataSet(data, result._1, result._2)
      writer.writeArff(attributeDataset, "./data/results/" + dataset._1 + "_CNN")
    }
  }
}