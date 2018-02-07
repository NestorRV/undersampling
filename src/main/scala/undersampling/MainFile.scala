package undersampling

import undersampling.core._
import undersampling.data.{ArffData, Data, DelimitedTextData}
import undersampling.io.{ArffReader, ArffWriter, DelimitedTextReader, DelimitedTextWriter}
import undersampling.util.Utilities.Distances

/** An object to test the different algorithms
  *
  * @author Néstor Rodríguez Vico
  */
object MainFile {
  def main(args: Array[String]): Unit = {
    val csv: Boolean = false

    if (csv) {
      val delimitedTextParse = new DelimitedTextReader
      val d: Data = delimitedTextParse.parse(file = "./input/wdbc.csv", comment = "#", delimiter = ",", missing = "", columnClass = -1, header = true)
      val writer: DelimitedTextWriter = new DelimitedTextWriter

      val ru = new RandomUndersampling(d)
      val resultRU: (Data, Array[Int]) = ru.sample(file = Option("./input/logs/wdbc"), numberOfElements = 10)
      writer.storeFile(file = "./input/results/wdbc_RU.csv", data = resultRU._1.asInstanceOf[DelimitedTextData])

      val cnn = new CondensedNearestNeighbor(d)
      val resultCNN: (Data, Array[Int]) = cnn.sample(file = Option("./input/logs/wdbc"), distance = Distances.EUCLIDEAN)
      writer.storeFile(file = "./input/results/wdbc_CNN.csv", data = resultCNN._1.asInstanceOf[DelimitedTextData])

      val enn = new EditedNearestNeighbor(d)
      val resultENN: (Data, Array[Int]) = enn.sample(file = Option("./input/logs/wdbc"), distance = Distances.EUCLIDEAN)
      writer.storeFile(file = "./input/results/wdbc_ENN.csv", data = resultENN._1.asInstanceOf[DelimitedTextData])

      val ncl = new NeighborhoodCleaningRule(d)
      val resultNCL: (Data, Array[Int]) = ncl.sample(file = Option("./input/logs/wdbc"), distance = Distances.EUCLIDEAN)
      writer.storeFile(file = "./input/results/wdbc_NCL.csv", data = resultNCL._1.asInstanceOf[DelimitedTextData])
    }
    else {
      val data = List("contact-lenses.arff", "diabetes.arff", "iris.arff", "vote.arff", "weather.nominal.arff")

      for (dataset <- data) {
        println(dataset)
        val arff = new ArffReader
        val d: Data = arff.parse(file = "input/" + dataset)
        val writer: ArffWriter = new ArffWriter

        val ru = new RandomUndersampling(d)
        val resultRU: (Data, Array[Int]) = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 10)
        writer.storeFile(file = "./input/results/" + dataset + "_RU", data = resultRU._1.asInstanceOf[ArffData])

        val cnn = new CondensedNearestNeighbor(d)
        val resultCNN: (Data, Array[Int]) = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
        writer.storeFile(file = "./input/results/" + dataset + "_CNN", data = resultCNN._1.asInstanceOf[ArffData])

        val enn = new EditedNearestNeighbor(d)
        val resultENN: (Data, Array[Int]) = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
        writer.storeFile(file = "./input/results/" + dataset + "_ENN", data = resultENN._1.asInstanceOf[ArffData])

        val ncl = new NeighborhoodCleaningRule(d)
        val resultNCL: (Data, Array[Int]) = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
        writer.storeFile(file = "./input/results/" + dataset + "_NCL", data = resultNCL._1.asInstanceOf[ArffData])
      }
    }
  }
}