package undersampling

import smile.data.{AttributeDataset, NominalAttribute}

/** An object to test the different algorithms
  *
  * @author Néstor Rodríguez Vico
  */
object MainFile {
  def main(args: Array[String]): Unit = {
    val readerArff: Reader = new Reader(dataSet = "./data/sonar.arff")
    val readerText: Reader = new Reader(dataSet = "./data/sonar.csv")
    val dataArff: AttributeDataset = readerArff.readArff(classColumn = 60)
    val dataText: AttributeDataset = readerText.readDelimitedText(delimiter = ",", header = true, response = Some((new NominalAttribute("class"), 60)))

    val writer = new Writer
    writer.writeArff(dataArff, "./data/results/sonar_arff")
    writer.writeDelimitedText(dataText, "./data/results/sona_csv", ",")
  }
}