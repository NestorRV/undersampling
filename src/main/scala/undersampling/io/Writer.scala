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

    if(data._fileInfo._attributes == null ||  data._fileInfo._attributesValues == null)
      throw new Exception("Unnable to write arff: missing information")

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
    val delimiter: String = if(data._fileInfo._delimiter == null) "," else data._fileInfo._delimiter
    val missing: String = if(data._fileInfo._missing == null) "?" else data._fileInfo._delimiter

    val pr = new PrintWriter(new File(file))
    if (data._fileInfo._header != null)
      pr.write(data._fileInfo._header.mkString(delimiter) + "\n")

    for (row <- data._resultData zip data._resultClasses) {
      val naIndex: Array[Int] = row._1.zipWithIndex.filter((_: (Any, Int))._1 == "undersampling_NA").map((_: (Any, Int))._2)
      val newRow: Array[Any] = row._1.clone()
      for (index <- naIndex) {
        newRow(index) = missing
      }

      pr.write(newRow.mkString(delimiter) + "," + row._2 + "\n")
    }

    pr.close()
  }
}