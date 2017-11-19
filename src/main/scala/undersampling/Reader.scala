package undersampling

import smile.data.{Attribute, AttributeDataset}

/** A simple class to read files
  *
  * @author Néstor Rodríguez Vico
  * @param dataSet a String representing the path of the file to read.
  */
class Reader(private val dataSet: String) {

  /** Reads an arff file into an AttributeDataset.
    *
    * @param classColumn index of the class columns.
    * @return an AttributeDataset containing the data.
    */
  def readArff(classColumn: Int): AttributeDataset = {
    smile.read.arff(dataSet, classColumn)
  }

  /** Reads a delimited text file into an AttributeDataset.
    *
    * @param attributes the list attributes of data in proper order.
    * @param response   optional response variable attribute and the column index of response variable. The column index starts at 0.
    * @param delimiter  delimiter string -> "," reads a CSV.
    * @param comment    the start of comment lines.
    * @param missing    the missing value placeholder.
    * @param header     true if the first row is header/column names.
    * @param rowNames   true if the first column is row id/names.
    * @return an AttributeDataset containing the data.
    */
  def readDelimitedText(attributes: Array[Attribute] = null, response: Option[(Attribute, Int)] = None,
                        delimiter: String = "\\s+", comment: String = "%", missing: String = "?",
                        header: Boolean = false, rowNames: Boolean = false): AttributeDataset = {

    smile.read.table(this.dataSet, attributes, response, delimiter, comment, missing, header, rowNames)
  }
}