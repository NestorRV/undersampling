package undersampling.io

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.text.ParseException

import scala.collection.mutable.ArrayBuffer

/** Class to read delimited text files (like a CSV for example)
  *
  * @author Néstor Rodríguez Vico
  */
class DelimitedTextParser {

  /** Parse a delimited text data file
    *
    * @param file        file containing the data
    * @param comment     string indicating that a line is a comment
    * @param delimiter   string separating two elements
    * @param missing     string indicating a element is missed
    * @param header      indicates if the file contains a header or not
    * @param columnClass indicates which column represents the class in the file
    * @return a Data object containing all the relevant information
    */
  def parse(file: String, comment: String, delimiter: String, missing: String, header: Boolean, columnClass: Int): Data = {
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
    reader.mark(1000000000)
    val firstLine: String = reader.readLine
    if (columnClass >= firstLine.split(delimiter).length) throw new ParseException("Invalid response variable index: " + columnClass, columnClass)
    reader.reset()

    val data = new Data
    if (header) data._header = reader.readLine.split(delimiter)
    else data._header = _: Array[String]

    data._file = file
    data._comment = comment
    data._delimiter = delimiter
    data._columnClass = columnClass

    var line: String = reader.readLine()
    val readData: ArrayBuffer[Array[Any]] = new ArrayBuffer[Array[Any]](0)
    val readClasses: ArrayBuffer[Any] = new ArrayBuffer[Any](0)

    while (line != null) {
      if (line.isEmpty || line.startsWith(comment)) {
        line = reader.readLine
      } else {
        val elements: Array[String] = line.split(delimiter)

        val row: ArrayBuffer[Any] = new ArrayBuffer[Any](0)
        for (e <- elements zip elements.indices.toList) {
          if (e._2 == columnClass) readClasses += e._1
          else if (e._1.matches("-?\\d+(\\.\\d+)?")) row += e._1.toDouble
          else if (e._1 == missing) row += Double.NaN
          else row += e._1
        }

        readData += row.toArray
        line = reader.readLine
      }
    }

    data._data = readData.toArray
    data._classes = readClasses.toArray
    data
  }
}
