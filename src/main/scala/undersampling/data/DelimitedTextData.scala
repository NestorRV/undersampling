package undersampling.data

/** Data structure used by the arff classes
  *
  * @param _file            file containing the data
  * @param _comment         string indicating that a line is a comment
  * @param _columnClass     indicates which column represents the class in the file
  * @param _nominal         array to know which classes are nominal
  * @param _originalData    data associated to the file (x)
  * @param _originalClasses classes associated to the file (y)
  * @param _delimiter       string separating two elements
  * @param _missing         string indicating a element is missed
  * @param _header          header of the file. If it is _, there was no header
  * @author Néstor Rodríguez Vico
  */
class DelimitedTextData private[undersampling](override private[undersampling] val _file: String, override private[undersampling] val _comment: String,
                                               override private[undersampling] val _columnClass: Int = -1, override private[undersampling] val _nominal: Array[Int],
                                               override private[undersampling] val _originalData: Array[Array[Any]],
                                               override private[undersampling] val _originalClasses: Array[Any], private[undersampling] val _delimiter: String,
                                               private[undersampling] val _missing: String,
                                               private[undersampling] val _header: Array[String]) extends Data(_file, _comment, _columnClass, _nominal, _originalData, _originalClasses) {
}
