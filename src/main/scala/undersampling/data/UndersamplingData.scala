package undersampling.data

/** UndersamplingData structure used by the algorithms
  *
  * @author Néstor Rodríguez Vico
  */
class UndersamplingData {
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
  // array to know which classes are nominal
  private[undersampling] var _nominal: Array[Int] = _
  // data associated to the file (x)
  private[undersampling] var _originalData: Array[Array[Any]] = _
  // classes associated to the file (y)
  private[undersampling] var _originalClasses: Array[Any] = _
  // data obtained after applying an algorithm
  private[undersampling] var _resultData: Array[Array[Double]] = _
  // classes obtained after applying an algorithm
  private[undersampling] var _resultClasses: Array[Any] = _


  /** originalData getter
    *
    * @return read data
    */
  def originalData: Array[Array[Any]] = _originalData

  /** originalClasses getter
    *
    * @return read classes
    */
  def originalClasses: Array[Any] = _originalClasses

  /** resultlData getter
    *
    * @return read data
    */
  def resultData: Array[Array[Double]] = _resultData

  /** resultClasses getter
    *
    * @return read classes
    */
  def resultClasses: Array[Any] = _resultClasses
}
