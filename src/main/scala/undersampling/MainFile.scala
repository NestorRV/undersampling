package undersampling

import undersampling.core._
import undersampling.data.Data
import undersampling.io.{Reader, Writer}
import undersampling.util.Utilities.Distances

/** An object to test the different algorithms
  *
  * @author Néstor Rodríguez Vico
  */
object MainFile {
  def main(args: Array[String]): Unit = {
    val delimitedTextData = List("abalone.data", "poker-hand-testing.data", "winequality-red.csv", "car.data", "spambase.data", "winequality-white.csv")
    val arffData = List("airlines.arff", "gisette.arff", "supermarket.arff", "amazon.arff", "madelon.arff")
    val reader = new Reader
    val writer: Writer = new Writer

    for (dataset <- delimitedTextData) {
      println(dataset)
      val d: Data = reader.readDelimitedText(file = "./input/delimited_text/" + dataset, comment = "#", delimiter = ",", missing = "", columnClass = -1, header = false)

      val cnn = new CondensedNearestNeighbor(d)
      println("CondensedNearestNeighbor")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_CNN.data", data = resultCNN)

      val enn = new EditedNearestNeighbor(d)
      println("EditedNearestNeighbor")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_ENN.data", data = resultENN)

      val ncl = new NeighborhoodCleaningRule(d)
      println("NeighborhoodCleaningRule")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_NCL.data", data = resultNCL)

      val oss = new OneSideSelection(d)
      println("OneSideSelection")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_OSS.data", data = resultOSS)

      val ru = new RandomUndersampling(d)
      println("RandomUndersampling")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 100)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_RU.data", data = resultRU)

      val tl = new TomekLink(d)
      println("TomekLink")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_TL.data", data = resultTL)
    }
  
    for (dataset <- arffData) {
      println(dataset)
      val d: Data = reader.readArff(file = "./input/arff/" + dataset)

      val cnn = new CondensedNearestNeighbor(d)
      println("CondensedNearestNeighbor")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeArff(file = "./input/results/" + dataset + "_CNN.data", data = resultCNN)

      val enn = new EditedNearestNeighbor(d)
      println("EditedNearestNeighbor")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeArff(file = "./input/results/" + dataset + "_ENN.data", data = resultENN)

      val ncl = new NeighborhoodCleaningRule(d)
      println("NeighborhoodCleaningRule")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeArff(file = "./input/results/" + dataset + "_NCL.data", data = resultNCL)

      val oss = new OneSideSelection(d)
      println("OneSideSelection")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeArff(file = "./input/results/" + dataset + "_OSS.data", data = resultOSS)

      val ru = new RandomUndersampling(d)
      println("RandomUndersampling")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 100)
      writer.writeArff(file = "./input/results/" + dataset + "_RU.data", data = resultRU)

      val tl = new TomekLink(d)
      println("TomekLink")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.writeArff(file = "./input/results/" + dataset + "_TL.data", data = resultTL)
    }
  }
}