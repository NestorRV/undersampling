package undersampling

import undersampling.core._
import undersampling.data.Data
import undersampling.io.{ArffReader, ArffWriter, DelimitedTextReader, DelimitedTextWriter}
import undersampling.util.Utilities.Distances

/** An object to test the different algorithms
  *
  * @author Néstor Rodríguez Vico
  */
object MainFile {
  def main(args: Array[String]): Unit = {
    val delimitedTextData = List("abalone.data", "poker-hand-testing.data", "winequality-red.csv", "car.data", "spambase.data", "winequality-white.csv")
    val arffData = List("airlines.arff", "gisette.arff", "supermarket.arff", "amazon.arff", "madelon.arff")

    for (dataset <- delimitedTextData) {
      println(dataset)
      val delimitedTextParse = new DelimitedTextReader
      val d: Data = delimitedTextParse.parse(file = "./input/delimited_text/" + dataset, comment = "#", delimiter = ",", missing = "", columnClass = -1, header = false)
      val writer: DelimitedTextWriter = new DelimitedTextWriter

      val cnn = new CondensedNearestNeighbor(d)
      println("CondensedNearestNeighbor")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_CNN.data", data = resultCNN)

      val enn = new EditedNearestNeighbor(d)
      println("EditedNearestNeighbor")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_ENN.data", data = resultENN)

      val ncl = new NeighborhoodCleaningRule(d)
      println("NeighborhoodCleaningRule")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_NCL.data", data = resultNCL)

      val oss = new OneSideSelection(d)
      println("OneSideSelection")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_OSS.data", data = resultOSS)

      val ru = new RandomUndersampling(d)
      println("RandomUndersampling")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 100)
      writer.storeFile(file = "./input/results/" + dataset + "_RU.data", data = resultRU)

      val tl = new TomekLink(d)
      println("TomekLink")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_TL.data", data = resultTL)
    }
  
    for (dataset <- arffData) {
      println(dataset)
      val arff = new ArffReader
      val d: Data = arff.parse(file = "./input/arff/" + dataset)
      val writer: ArffWriter = new ArffWriter

      val cnn = new CondensedNearestNeighbor(d)
      println("CondensedNearestNeighbor")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_CNN.data", data = resultCNN)

      val enn = new EditedNearestNeighbor(d)
      println("EditedNearestNeighbor")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_ENN.data", data = resultENN)

      val ncl = new NeighborhoodCleaningRule(d)
      println("NeighborhoodCleaningRule")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_NCL.data", data = resultNCL)

      val oss = new OneSideSelection(d)
      println("OneSideSelection")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_OSS.data", data = resultOSS)

      val ru = new RandomUndersampling(d)
      println("RandomUndersampling")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 100)
      writer.storeFile(file = "./input/results/" + dataset + "_RU.data", data = resultRU)

      val tl = new TomekLink(d)
      println("TomekLink")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/" + dataset + "_TL.data", data = resultTL)
    }
  }
}