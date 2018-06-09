package undersampling.io

import java.io.{File, PrintWriter}

import undersampling.data.Data

import scala.collection.immutable.ListMap

/** Class write data files
  *
  * @author Néstor Rodríguez Vico
  */
class Writer {
  /** Store data into a delimited text file
    *
    * @param file filename where to store the data
    * @param data data to save to the file
    */
  def writeArff(file: String, data: Data): Unit = {
    val pr = new PrintWriter(new File(file))
    pr.write("@relation %s\n".format(data._fileInfo._relationName))

    val orderedAttributes: Map[Int, String] = ListMap(data._fileInfo._attributes.toSeq.sortBy((_: (Int, String))._1): _*)

    for (attribute <- orderedAttributes) {
      pr.write("@attribute %s %s\n".format(attribute._2, data._fileInfo._attributesValues(attribute._2)))
    }

    pr.write("@data\n")

    for (row <- data._resultData zip data._resultClasses) {
      val naIndex: Array[Int] = row._1.zipWithIndex.filter((_: (Any, Int))._1 == "undersampling_NA").map((_: (Any, Int))._2)
      val newRow: Array[Any] = row._1.clone()
      for (index <- naIndex) {
        newRow(index) = "?"
      }

      pr.write(newRow.mkString(",") + "," + row._2 + "\n")
    }

    pr.close()
  }

  /** Store data into a delimited text file
    *
    * @param file filename where to store the data
    * @param data data to save to the file
    */
  def writeDelimitedText(file: String, data: Data): Unit = {
    val pr = new PrintWriter(new File(file))
    if (data._fileInfo._header.length != 0)
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