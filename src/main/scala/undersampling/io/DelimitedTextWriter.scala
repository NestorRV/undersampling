package undersampling.io

import java.io.{File, PrintWriter}

import undersampling.data.Data

/** Class to store delimited text files (like a CSV for example)
  *
  * @author Néstor Rodríguez Vico
  */
class DelimitedTextWriter {

  /** Store data into a delimited text file
    *
    * @param file filename where to store the logs
    * @param data data to save to the file
    */
  def storeFile(file: String, data: Data): Unit = {
    val pr = new PrintWriter(new File(file))
    if (data._fileInfo._header != null)
      pr.write(data._fileInfo._header.mkString(data._fileInfo._delimiter) + "\n")

    for (row <- data._resultData zip data._resultClasses) {
      val naIndex: Array[Int] = row._1.zipWithIndex.filter((_: (Any, Int))._1 == "undersampling_NA").map((_: (Any, Int))._2)
      val newRow: Array[Any] = row._1.clone()
      for (index <- naIndex) {
        newRow(index) = data._fileInfo._missing
      }

      pr.write(newRow.mkString(data._fileInfo._delimiter) + "," + row._2 + "\n")
    }

    pr.close()
  }
}