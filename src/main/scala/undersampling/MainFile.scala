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
    val delimitedTextData = List("abalone.data", "car.data", "spambase.data", "wdbc.csv")
    val arffData = List("madelon.arff")
    val reader = new Reader
    val writer: Writer = new Writer

    for (dataset <- delimitedTextData) {
      println(dataset)
      val d: Data = reader.readDelimitedText(file = "./input/delimited_text/" + dataset, comment = "#", delimiter = ",", missing = "", columnClass = -1, header = false)

      val cnn = new CondensedNearestNeighbor(d, seed = 0L)
      println("CondensedNearestNeighbor")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_CNN.data", data = resultCNN)

      val enn = new EditedNearestNeighbor(d, seed = 0L)
      println("EditedNearestNeighbor")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_ENN.data", data = resultENN)

      val ncl = new NeighborhoodCleaningRule(d, seed = 0L)
      println("NeighborhoodCleaningRule")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_NCL.data", data = resultNCL)

      val oss = new OneSideSelection(d, seed = 0L)
      println("OneSideSelection")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_OSS.data", data = resultOSS)

      val ru = new RandomUndersampling(d, seed = 0L)
      println("RandomUndersampling")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 100)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_RU.data", data = resultRU)

      val tl = new TomekLink(d, seed = 0L)
      println("TomekLink")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeDelimitedText(file = "./input/results/" + dataset + "_TL.data", data = resultTL)
    }

    for (dataset <- arffData) {
      println(dataset)
      val d: Data = reader.readArff(file = "./input/arff/" + dataset)

      val cnn = new CondensedNearestNeighbor(d, seed = 0L)
      println("CondensedNearestNeighbor")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeArff(file = "./input/results/" + dataset + "_CNN.data", data = resultCNN)

      val enn = new EditedNearestNeighbor(d, seed = 0L)
      println("EditedNearestNeighbor")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeArff(file = "./input/results/" + dataset + "_ENN.data", data = resultENN)

      val ncl = new NeighborhoodCleaningRule(d, seed = 0L)
      println("NeighborhoodCleaningRule")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeArff(file = "./input/results/" + dataset + "_NCL.data", data = resultNCL)

      val oss = new OneSideSelection(d, seed = 0L)
      println("OneSideSelection")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeArff(file = "./input/results/" + dataset + "_OSS.data", data = resultOSS)

      val ru = new RandomUndersampling(d, seed = 0L)
      println("RandomUndersampling")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 100)
      writer.writeArff(file = "./input/results/" + dataset + "_RU.data", data = resultRU)

      val tl = new TomekLink(d, seed = 0L)
      println("TomekLink")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.HVDM)
      writer.writeArff(file = "./input/results/" + dataset + "_TL.data", data = resultTL)
    }
  }
}