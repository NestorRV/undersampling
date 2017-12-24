package undersampling

import smile.data.AttributeDataset
import undersampling.algorithm.CNN
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
      val cnn = new CNN(data)
      val result: AttributeDataset = cnn.compute(file = "./data/logs/" + dataset._1, distance = Distances.EUCLIDEAN)
      val writer = new Writer
      writer.writeArff(result, "./data/results/" + dataset._1)
    }
  }
}