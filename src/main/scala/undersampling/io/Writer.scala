package undersampling.io

import smile.data.AttributeDataset

/** A simple class to write data into text files
  *
  * @author Néstor Rodríguez Vico
  */
class Writer {
  /** Writes an AttributeDataset to a arff file.
    *
    * @param data data to be stored.
    * @param file file path to store the data.
    */
  def writeArff(data: AttributeDataset, file: String): Unit = {
    smile.write.arff(data, file)
  }

  /** Writes an AttributeDataset to a text file.
    *
    * @param data      data to be stored.
    * @param file      file path to store the data.
    * @param delimiter separator between two values. With "," you will write a CSV file.
    */
  def writeDelimitedText(data: AttributeDataset, file: String, delimiter: String = "\t"): Unit = {
    smile.write.table(data, file, delimiter)
  }

  /** Build an AttributeDataset with the data provided in x and the classes provided in y
    *
    * @param originalData original AttributeDataset to read the meta info
    * @param x            data to store in the AttributeDataset
    * @param y            labels associated to x
    * @return the final AttributeDataset
    */
  def toDataSet(originalData: AttributeDataset, x: Array[Array[Double]], y: Array[Int]): AttributeDataset = {
    val dataSet: AttributeDataset = new AttributeDataset(originalData.getName, originalData.attributes(), originalData.response().attribute())
    (x zip y).foreach((pair: (Array[Double], Int)) => dataSet.add(pair._1, pair._2))
    dataSet
  }
}
