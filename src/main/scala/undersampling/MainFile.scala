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

      val cnn = new CNN(d, seed = 0L)
      println("CNN")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_CNN.data", data = resultCNN)

      val cpm = new CPM(d, seed = 0L)
      println("CPM")
      val resultCPM: Data = cpm.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_CPM.data", data = resultCPM)

      val enn = new ENN(d, seed = 0L)
      println("ENN")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_ENN.data", data = resultENN)

      val ncl = new NCL(d, seed = 0L)
      println("NCL")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_NCL.data", data = resultNCL)

      val oss = new OSS(d, seed = 0L)
      println("OSS")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_OSS.data", data = resultOSS)

      val ru = new RU(d, seed = 0L)
      println("RU")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 100)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_RU.data", data = resultRU)

      val sbc = new SBC(d, seed = 0L)
      println("SBC")
      val resultSBC: Data = sbc.sample(file = Option("./input/logs/" + dataset), numClusters = 25)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_SBC.data", data = resultSBC)

      val tl = new TL(d, seed = 0L)
      println("TL")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_TL.data", data = resultTL)
    }

    for (dataset <- arffData) {
      println(dataset)
      val d: Data = reader.readArff(file = "./input/arff/" + dataset)

      val cnn = new CNN(d, seed = 0L)
      println("CNN")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_CNN.data", data = resultCNN)

      val cpm = new CPM(d, seed = 0L)
      println("CPM")
      val resultCPM: Data = cpm.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_CPM.data", data = resultCPM)

      val enn = new ENN(d, seed = 0L)
      println("ENN")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_ENN.data", data = resultENN)

      val ncl = new NCL(d, seed = 0L)
      println("NCL")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_NCL.data", data = resultNCL)

      val oss = new OSS(d, seed = 0L)
      println("OSS")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_OSS.data", data = resultOSS)

      val ru = new RU(d, seed = 0L)
      println("RU")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 100)
      // writer.writeArff(file = "./input/results/" + dataset + "_RU.data", data = resultRU)

      val sbc = new SBC(d, seed = 0L)
      println("SBC")
      val resultSBC: Data = sbc.sample(file = Option("./input/logs/" + dataset), numClusters = 25)
      // writer.writeArff(file = "./input/results/" + dataset + "_SBC.data", data = resultSBC)

      val tl = new TL(d, seed = 0L)
      println("TL")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_TL.data", data = resultTL)
    }
  }
}