package undersampling

import undersampling.core._
import undersampling.data.UndersamplingData
import undersampling.io.{DelimitedTextReader, DelimitedTextWriter}
import undersampling.util.Utilities.Distances

/** An object to test the different algorithms
  *
  * @author Néstor Rodríguez Vico
  */
object MainFile {
  def main(args: Array[String]): Unit = {
    val delimitedTextParse = new DelimitedTextReader
    val d: UndersamplingData = delimitedTextParse.parse(file = "./data/wdbc_1.csv", comment = "#", delimiter = ",", missing = "", columnClass = -1, header = true)
    val writer: DelimitedTextWriter = new DelimitedTextWriter

    val ru = new RandomUndersampling(d)
    val resultRU: (UndersamplingData, Array[Int]) = ru.sample(file = Option("./data/logs/wdbc_1"), numberOfElements = 10)
    writer.storeFile(file = "./data/results/wdbc_1_RU.csv", data = resultRU._1)

    val cnn = new CondensedNearestNeighbor(d)
    val resultCNN: (UndersamplingData, Array[Int]) = cnn.sample(file = Option("./data/logs/wdbc_1"), distance = Distances.EUCLIDEAN)
    writer.storeFile(file = "./data/results/wdbc_1_CNN.csv", data = resultCNN._1)

    val enn = new EditedNearestNeighbor(d)
    val resultENN: (UndersamplingData, Array[Int]) = enn.sample(file = Option("./data/logs/wdbc_1"), distance = Distances.EUCLIDEAN, k = 3)
    writer.storeFile(file = "./data/results/wdbc_1_ENN.csv", data = resultENN._1)

    val ncl = new NeighborhoodCleaningRule(d)
    val resultNCL: (UndersamplingData, Array[Int]) = ncl.sample(file = Option("./data/logs/wdbc_1"), distance = Distances.EUCLIDEAN)
    writer.storeFile(file = "./data/results/wdbc_1_NCL.csv", data = resultNCL._1)
  }
}