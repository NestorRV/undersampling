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
    val csv: Boolean = true

    if (csv) {
      val delimitedTextParse = new DelimitedTextReader
      val d: Data = delimitedTextParse.parse(file = "./input/spambase.data", comment = "#", delimiter = ",", missing = "", columnClass = -1, header = false)
      val writer: DelimitedTextWriter = new DelimitedTextWriter

      val cnn = new CondensedNearestNeighbor(d)
      val resultCNN: Data = cnn.sample(file = Option("./input/logs/spambase"), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/spambase_CNN.data", data = resultCNN.asInstanceOf[DelimitedTextData])

      val enn = new EditedNearestNeighbor(d)
      val resultENN: Data = enn.sample(file = Option("./input/logs/spambase"), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/spambase_ENN.data", data = resultENN.asInstanceOf[DelimitedTextData])

      val ncl = new NeighborhoodCleaningRule(d)
      val resultNCL: Data = ncl.sample(file = Option("./input/logs/spambase"), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/spambase_NCL.data", data = resultNCL.asInstanceOf[DelimitedTextData])

      val ru = new RandomUndersampling(d)
      val resultRU: Data = ru.sample(file = Option("./input/logs/spambase"), numberOfElements = 10)
      writer.storeFile(file = "./input/results/spambase_RU.data", data = resultRU.asInstanceOf[DelimitedTextData])

      val tl = new TomekLink(d)
      val resultTL: Data = tl.sample(file = Option("./input/logs/spambase"), distance = Distances.EUCLIDEAN_NOMINAL)
      writer.storeFile(file = "./input/results/spambase_TL.data", data = resultTL.asInstanceOf[DelimitedTextData])
    }
    else {
      val data = List("contact-lenses.arff", "diabetes.arff", "iris.arff", "vote.arff", "weather.nominal.arff")

      for (dataset <- data) {
        println(dataset)
        val arff = new ArffReader
        val d: Data = arff.parse(file = "input/" + dataset)
        val writer: ArffWriter = new ArffWriter

        val cnn = new CondensedNearestNeighbor(d)
        val resultCNN: Data = cnn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
        writer.storeFile(file = "./input/results/" + dataset + "_CNN", data = resultCNN.asInstanceOf[ArffData])

        val enn = new EditedNearestNeighbor(d)
        val resultENN: Data = enn.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
        writer.storeFile(file = "./input/results/" + dataset + "_ENN", data = resultENN.asInstanceOf[ArffData])

        val ncl = new NeighborhoodCleaningRule(d)
        val resultNCL: Data = ncl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
        writer.storeFile(file = "./input/results/" + dataset + "_NCL", data = resultNCL.asInstanceOf[ArffData])

        val ru = new RandomUndersampling(d)
        val resultRU: Data = ru.sample(file = Option("./input/logs/" + dataset), numberOfElements = 10)
        writer.storeFile(file = "./input/results/" + dataset + "_RU", data = resultRU.asInstanceOf[ArffData])

        val tl = new TomekLink(d)
        val resultTL: Data = tl.sample(file = Option("./input/logs/" + dataset), distance = Distances.EUCLIDEAN_NOMINAL)
        writer.storeFile(file = "./input/results/" + dataset + "_TL", data = resultTL.asInstanceOf[ArffData])
      }
    }
  }
}