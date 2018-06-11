package undersampling.io

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.text.ParseException

import undersampling.data.{Data, FileInfo}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Class to read data files
  *
  * @author Néstor Rodríguez Vico
  */
class Reader {
  /** Parse a arff file
    *
    * @param file        file containing the data
    * @param columnClass indicates which column represents the class in the file. It it's set to -1, it will take the las column
    * @return a Data object containing all the relevant information
    */
  def readArff(file: String, columnClass: Int = -1): Data = {
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
    var line: String = reader.readLine()
    var relationName: String = ""
    // index -> attributeName
    val attributes: mutable.Map[Int, String] = collection.mutable.Map[Int, String]()
    // attributeName -> type (it it's nominal, possible values instead of type)
    val attributesValues: mutable.Map[String, String] = collection.mutable.Map[String, String]()

    var dataDetected: Boolean = false
    var counter: Int = 0

    while (line != null && !dataDetected) {
      // ignore comments/description lines
      if (line.isEmpty || line.startsWith("%")) {
        line = reader.readLine
      } else {
        // take care if the relation name has commas, tabs, multiple spaces...
        val parts: Array[String] = line.replaceAll("\t", " ").replaceAll("\\s{2,}", " ").split(" ", 3)
        if (parts(0).equalsIgnoreCase("@relation")) {
          // drop the identifier and group all the possible parts separated by a space
          relationName = parts.drop(1).mkString(" ")
        } else if (parts(0).equalsIgnoreCase("@attribute")) {
          attributes += (counter -> parts(1))
          attributesValues += (parts(1) -> parts(2))
          counter += 1
        } else if (parts(0).equalsIgnoreCase("@data")) {
          dataDetected = true
        }

        line = reader.readLine
      }
    }

    if (columnClass >= attributes.size)
      throw new ParseException("Invalid response variable index: " + columnClass, columnClass)

    val response: Int = if (columnClass == -1) attributes.size - 1 else columnClass
    val readData: ArrayBuffer[Array[String]] = new ArrayBuffer[Array[String]](0)

    // Now we have the attributes, let's save the data
    while (line != null) {
      if (line.isEmpty || line.startsWith("%")) {
        line = reader.readLine
      } else {
        val parts: Array[String] = line.replaceAll("\t", " ").replaceAll("\\s{2,}", " ").split(",")
        // there are not quotations mark
        if (parts.length == (attributes.size + 1)) {
          readData += parts.asInstanceOf
        } else {
          // if there are quotations marks, they are going to be in pairs
          val subParts: Array[Array[Int]] = parts.zipWithIndex.filter((x: (String, Int)) => x._1.contains("\"")).collect { case (_, a) => a }.grouped(2).toArray
          // separators indicates the index of the elements that need to be merged into one class
          val separators = new ArrayBuffer[Array[Int]](0)
          for (quotationMarks <- subParts)
            separators += (quotationMarks(0) to quotationMarks(1)).toArray

          val separatedValues: ArrayBuffer[String] = new ArrayBuffer[String]()
          // append all the parts into one value
          for (pair <- subParts)
            separatedValues += ((pair(0) to pair(1)).toArray map parts).mkString(",")

          val nonSeparatedValuesIndex: Array[Int] = parts.indices.diff(separators.flatten.toList).toArray
          val nonSeparatedValues: Array[String] = nonSeparatedValuesIndex map parts
          // append all the data
          val values: Array[String] = (separatedValues ++ nonSeparatedValues).toArray
          // make an index array merging all the index: take care with the separatedValuesIndex because there are more than one
          // index for each value, so we compute the mean for all the numbers associated to one value
          val index: Array[Double] = separators.map((a: Array[Int]) => a.sum.toDouble / a.length).toArray ++ nonSeparatedValuesIndex.map((_: Int).asInstanceOf[Double])
          // finally, construct an array to sort the values
          val indexForMap: Array[Int] = index.zipWithIndex.sortBy((pair: (Double, Int)) => pair._1).map((pair: (Double, Int)) => pair._2)
          // get the final values
          val finalValues: Array[String] = indexForMap map values
          if (finalValues.length != attributes.size)
            throw new ParseException("%d columns, expected %d".format((indexForMap map values).length, attributes.size), (indexForMap map values).length)

          readData += finalValues
        }
        line = reader.readLine
      }
    }

    val finalData: ArrayBuffer[Array[Any]] = new ArrayBuffer[Array[Any]](0)
    val readClasses: ArrayBuffer[Any] = new ArrayBuffer[Any](0)
    val readNominal: ArrayBuffer[Int] = new ArrayBuffer[Int](0)

    for (row <- readData) {
      val r = new ArrayBuffer[Any](0)
      for (e <- row.zipWithIndex) {
        if (e._2 == response)
          readClasses += e._1
        else if (e._1.matches("-?\\d+(\\.\\d+)?"))
          r += e._1.toDouble
        else {
          if (e._1 == "?" || e._1 == "'?'")
            r += "undersampling_NA"
          else {
            r += e._1
            readNominal += (if (e._2 >= response) e._2 - 1 else e._2)
          }
        }
      }

      finalData += r.toArray
    }

    val fileInfo = new FileInfo(_file = file, _comment = "%", _columnClass = response, _delimiter = null, _missing = "?", _header = null,
      _relationName = relationName, _attributes = attributes, _attributesValues = attributesValues)
    new Data(_nominal = readNominal.distinct.toArray, _originalData = finalData.toArray, _originalClasses = readClasses.toArray, _fileInfo = fileInfo)
  }

  /** Parse a delimited text data file
    *
    * @param file        file containing the data
    * @param comment     string indicating that a line is a comment
    * @param delimiter   string separating two elements
    * @param missing     string indicating a element is missed
    * @param header      indicates if the file contains a header or not
    * @param columnClass indicates which column represents the class in the file. It it's set to -1, it will take the las column
    * @return a Data object containing all the relevant information
    */
  def readDelimitedText(file: String, comment: String = "#", delimiter: String = ",", missing: String = "?", header: Boolean = true, columnClass: Int = -1): Data = {
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
    reader.mark(1000000000)
    val firstLine: String = reader.readLine
    if (columnClass >= firstLine.split(delimiter).length) throw new ParseException("Invalid response variable index: " + columnClass, columnClass)
    val response: Int = if (columnClass == -1) firstLine.split(delimiter).length - 1 else columnClass
    reader.reset()

    val headerArray: Array[String] = if (header) reader.readLine.split(delimiter) else null
    var line: String = reader.readLine
    val readData: ArrayBuffer[Array[Any]] = new ArrayBuffer[Array[Any]](0)
    val readClasses: ArrayBuffer[Any] = new ArrayBuffer[Any](0)
    val readNominal: ArrayBuffer[Int] = new ArrayBuffer[Int](0)

    while (line != null) {
      if (line.isEmpty || line.startsWith(comment)) {
        line = reader.readLine
      } else {
        val elements: Array[String] = line.split(delimiter)

        if (elements.length != firstLine.split(delimiter).length)
          throw new ParseException("%d columns, expected %d".format(elements.length, firstLine.length), elements.length)

        val row = new ArrayBuffer[Any](0)
        for (e <- elements.zipWithIndex) {
          if (e._2 == response)
            readClasses += e._1
          else if (e._1.replaceAll("\\s", "").matches("-?\\d+(\\.\\d+)?"))
            row += e._1.replaceAll("\\s", "").toDouble
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

    val fileInfo = new FileInfo(_file = file, _comment = "%", _columnClass = response, _delimiter = delimiter, _missing = missing, _header = headerArray, _relationName = null,
      _attributes = null, _attributesValues = null)
    new Data(_nominal = readNominal.distinct.toArray, _originalData = readData.toArray, _originalClasses = readClasses.toArray, _fileInfo = fileInfo)
  }
}
