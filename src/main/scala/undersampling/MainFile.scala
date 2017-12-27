package undersampling

import smile.data.AttributeDataset
import undersampling.core.Algorithm
import undersampling.io.{Reader, Writer}
import undersampling.util.Utilities.Distances

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
      val algorithm = new Algorithm(data)

      /*
      val result: AttributeDataset = algorithm.RandomUndersampling(file = "./data/logs/" + dataset._1, numberOfElements = 10)
      val writer = new Writer
      writer.writeArff(result, "./data/results/" + dataset._1 + "_RU")
      */

      val result: AttributeDataset = algorithm.CNN(file = "./data/logs/" + dataset._1, distance = Distances.SAMELABELS)
      val writer = new Writer
      writer.writeArff(result, "./data/results/" + dataset._1 + "_CNN")
    }
  }
}