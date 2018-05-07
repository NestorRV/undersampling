package undersampling.data

import scala.collection.mutable

/** Data structure used by the arff classes
  *
  * @param _file             file containing the data
  * @param _comment          string indicating that a line is a comment
  * @param _columnClass      indicates which column represents the class in the file
  * @param _delimiter        string separating two elements
  * @param _missing          string indicating a element is missed
  * @param _header           header of the file. If it is _, there was no header
  * @param _attributes       map with the form: index -> attributeName
  * @param _attributesValues map with the form attributeName -> type (it it's nominal, possible values instead of type)
  * @author Néstor Rodríguez Vico
  */
class FileInfo private[undersampling](private[undersampling] val _file: String, private[undersampling] val _comment: String,
                                      private[undersampling] val _columnClass: Int = -1,
                                      private[undersampling] val _delimiter: String, private[undersampling] val _missing: String,
                                      private[undersampling] val _header: Array[String], private[undersampling] val _relationName: String,
                                      private[undersampling] val _attributes: mutable.Map[Int, String],
                                      private[undersampling] val _attributesValues: mutable.Map[String, String]) {

  /** Return a deep copy of the object
    *
    * @return copy of the actual object
    */
  def copy(): FileInfo = {
    new FileInfo(this._file,this._comment, this._columnClass, this._delimiter, this._missing,
      this._header.clone, this._relationName, this._attributes.clone, this._attributesValues.clone)
  }

}