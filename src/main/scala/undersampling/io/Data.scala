package undersampling.io

/** Data read by the parsers
  *
  * @author Néstor Rodríguez Vico
  */
class Data {
  // file containing the data
  private[undersampling] var _file: String = ""
  // string indicating that a line is a comment
  private[undersampling] var _comment: String = ""
  // string separating two elements
  private[undersampling] var _delimiter: String = ""
  // string indicating a element is missed
  private[undersampling] var _missing: String = ""
  // header of the file. If it is _, there was no header
  private[undersampling] var _header: Array[String] = _
  // indicates which column represents the class in the file
  private[undersampling] var _columnClass: Int = -1
  // data associated to the file (x)
  private[undersampling] var _data: Array[Array[Any]] = _
  // classes associated to the file (y)
  private[undersampling] var _classes: Array[Any] = _

  /** data getter
    *
    * @return read data
    */
  def data: Array[Array[Any]] = _data

  /** classes getter
    *
    * @return read classes
    */
  def classes: Array[Any] = _classes
}
