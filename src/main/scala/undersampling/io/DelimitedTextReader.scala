package undersampling.io

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.text.ParseException

import undersampling.data.UndersamplingData

import scala.collection.mutable.ArrayBuffer

/** Class to read delimited text files (like a CSV for example)
  *
  * @author Néstor Rodríguez Vico
  */
class DelimitedTextReader {

  /** Parse a delimited text data file
    *
    * @param file        file containing the data
    * @param comment     string indicating that a line is a comment
    * @param delimiter   string separating two elements
    * @param missing     string indicating a element is missed
    * @param header      indicates if the file contains a header or not
    * @param columnClass indicates which column represents the class in the file. It it's set to -1, it will take the las column
    * @return a UndersamplingData object containing all the relevant information
    */
  def parse(file: String, comment: String, delimiter: String, missing: String, header: Boolean, columnClass: Int = -1): UndersamplingData = {
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
    reader.mark(1000000000)
    val firstLine: String = reader.readLine
    if (columnClass >= firstLine.split(delimiter).length) throw new ParseException("Invalid response variable index: " + columnClass, columnClass)
    val response: Int = if (columnClass == -1) firstLine.split(delimiter).length - 1 else columnClass
    reader.reset()

    val data = new UndersamplingData
    if (header)
      data._header = reader.readLine.split(delimiter)
    else
      data._header = _: Array[String]

    data._file = file
    data._comment = comment
    data._delimiter = delimiter
    data._columnClass = response

    var line: String = reader.readLine()
    val readData: ArrayBuffer[Array[Any]] = new ArrayBuffer[Array[Any]](0)
    val readClasses: ArrayBuffer[Any] = new ArrayBuffer[Any](0)
    val readNominal: ArrayBuffer[Int] = new ArrayBuffer[Int](0)

    while (line != null) {
      if (line.isEmpty || line.startsWith(comment)) {
        line = reader.readLine
      } else {
        val elements: Array[String] = line.split(delimiter)

        if (elements.length != data._header.length)
          throw new ParseException("%d columns, expected %d".format(elements.length, data._header.length), elements.length)

        val row = new ArrayBuffer[Any](0)
        for (e <- elements.zipWithIndex) {
          if (e._2 == response)
            readClasses += e._1
          else if (e._1.matches("-?\\d+(\\.\\d+)?"))
            row += e._1.toDouble
          else {
            if (e._1 == missing)
              row += "undersampling_NA"
            else {
              row += e._1
              readNominal += (if (e._2 >= response) e._2 - 1 else e._2)
            }
          }
        }

        readData += row.toArray
        line = reader.readLine
      }
    }

    data._originalData = readData.toArray
    data._originalClasses = readClasses.toArray
    data._nominal = readNominal.distinct.toArray
    data
  }
}
