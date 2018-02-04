package undersampling.io

import java.io.{File, PrintWriter}

import undersampling.data.UndersamplingData

class DelimitedTextWriter {

  /** Store data into a delimited text file
    *
    * @param file filename where to store the logs
    * @param data data to save to the file
    */
  def storeFile(file: String, data: UndersamplingData): Unit = {
    val pr = new PrintWriter(new File(file))
    pr.write(data._header.mkString(data._delimiter) + "\n")

    for (row <- data._resultData zip data._resultClasses) {
      val naIndex: Array[Int] = row._1.zipWithIndex.filter((_: (Any, Int))._1 == "undersampling_NA").map((_: (Any, Int))._2)
      val newRow: Array[Any] = row._1.clone()
      for(index <- naIndex){
        newRow(index) = data._missing
      }

      pr.write(newRow.mkString(data._delimiter) + "," + row._2 + "\n")
    }

    pr.close()
  }
}