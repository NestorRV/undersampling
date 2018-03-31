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

      val cpm = new ClassPurityMaximization(d, seed = 0L)
      println("ClassPurityMaximization")
      val resultCPM: Data = cpm.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_CPM.data", data = resultCPM)

      val cOSS = new ClusterOSS(d, seed = 0L)
      println("ClusterOSS")
      val resultCOSS: Data = cOSS.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN, numClusters = 50)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_ClusterOSS.data", data = resultCOSS)

      val cnn = new CondensedNearestNeighbor(d, seed = 0L)
      println("CondensedNearestNeighbor")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_CNN.data", data = resultCNN)

      val ee = new EasyEnsemble(d, seed = 0L)
      println("EasyEnsemble")
      val resultEE: Data = ee.sample(file = Option("./input/logs/" + dataset), n_times = 5)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_EE.data", data = resultEE)

      val enn = new EditedNearestNeighbor(d, seed = 0L)
      println("EditedNearestNeighbor")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_ENN.data", data = resultENN)

      val nm = new NearMiss(d, seed = 0L)
      println("NearMiss")
      val resultNM: Data = nm.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN, version = 3, n_neighbours = 3, ratio = 1.0)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_NM.data", data = resultNM)

      val ncl = new NeighbourhoodCleaningRule(d, seed = 0L)
      println("NeighbourhoodCleaningRule")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_NCL.data", data = resultNCL)

      val oss = new OneSideSelection(d, seed = 0L)
      println("OneSideSelection")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_OSS.data", data = resultOSS)

      val ru = new RandomUndersampling(d, seed = 0L)
      println("RandomUndersampling")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), ratio = 1.0)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_RU.data", data = resultRU)

      val tl = new TomekLink(d, seed = 0L)
      println("TomekLink")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_TL.data", data = resultTL)

      val sbc = new UndersamplingBasedClustering(d, seed = 0L)
      println("UndersamplingBasedClustering")
      val resultSBC: Data = sbc.sample(file = Option("./input/logs/" + dataset), numClusters = 25)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_SBC.data", data = resultSBC)
    }

    for (dataset <- arffData) {
      println(dataset)
      val d: Data = reader.readArff(file = "./input/arff/" + dataset)

      val cpm = new ClassPurityMaximization(d, seed = 0L)
      println("ClassPurityMaximization")
      val resultCPM: Data = cpm.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeDelimitedText(file = "./input/results/" + dataset + "_CPM.arff", data = resultCPM)

      val cOSS = new ClusterOSS(d, seed = 0L)
      println("ClusterOSS")
      val resultCOSS: Data = cOSS.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN, numClusters = 50)
      // writer.writeArff(file = "./input/results/" + dataset + "_ClusterOSS.arff", data = resultCOSS)

      val cnn = new CondensedNearestNeighbor(d, seed = 0L)
      println("CondensedNearestNeighbor")
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_CNN.arff", data = resultCNN)

      val ee = new EasyEnsemble(d, seed = 0L)
      println("EasyEnsemble")
      val resultEE: Data = ee.sample(file = Option("./input/logs/" + dataset), n_times = 5)
      // writer.writeArff(file = "./input/results/" + dataset + "_EE.arff", data = resultEE)

      val enn = new EditedNearestNeighbor(d, seed = 0L)
      println("EditedNearestNeighbor")
      val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_ENN.arff", data = resultENN)

      val nm = new NearMiss(d, seed = 0L)
      println("NearMiss")
      val resultNM: Data = nm.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN, version = 3, n_neighbours = 3)
      // writer.writeArff(file = "./input/results/" + dataset + "_NM.arff", data = resultNM)

      val ncl = new NeighbourhoodCleaningRule(d, seed = 0L)
      println("NeighbourhoodCleaningRule")
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_NCL.arff", data = resultNCL)

      val oss = new OneSideSelection(d, seed = 0L)
      println("OneSideSelection")
      val resultOSS: Data = oss.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_OSS.arff", data = resultOSS)

      val ru = new RandomUndersampling(d, seed = 0L)
      println("RandomUndersampling")
      val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset))
      // writer.writeArff(file = "./input/results/" + dataset + "_RU.arff", data = resultRU)

      val tl = new TomekLink(d, seed = 0L)
      println("TomekLink")
      val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN)
      // writer.writeArff(file = "./input/results/" + dataset + "_TL.arff", data = resultTL)

      val sbc = new UndersamplingBasedClustering(d, seed = 0L)
      println("UndersamplingBasedClustering")
      val resultSBC: Data = sbc.sample(file = Option("./input/logs/" + dataset), numClusters = 25)
      // writer.writeArff(file = "./input/results/" + dataset + "_SBC.arff", data = resultSBC)
    }
  }
}