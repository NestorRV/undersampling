package undersampling.io

import java.io.{File, PrintWriter}

import undersampling.data.ArffData

import scala.collection.immutable.ListMap

/** Class to store delimited text files (like a CSV for example)
  *
  * @author Néstor Rodríguez Vico
  */
class ArffWriter {
  /** Store data into a delimited text file
    *
    * @param file filename where to store the logs
    * @param data data to save to the file
    */
  def storeFile(file: String, data: ArffData): Unit = {
    val pr = new PrintWriter(new File(file))
    pr.write("@relation %s\n".format(data._relationName))

    val orderedAttributes: Map[Int, String] = ListMap(data._attributes.toSeq.sortBy((_: (Int, String))._1): _*)

    for (attribute <- orderedAttributes) {
      pr.write("@attribute %s %s\n".format(attribute._2, data._attributesValues(attribute._2)))
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
}