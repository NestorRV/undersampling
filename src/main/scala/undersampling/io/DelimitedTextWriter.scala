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
      pr.write(row._1.mkString(data._delimiter) + "," + row._2 + "\n")
    }

    pr.close()
  }
}