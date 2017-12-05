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
}
