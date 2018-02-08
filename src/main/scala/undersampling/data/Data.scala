package undersampling.data

/** Data structure used by the algorithms
  *
  * @param _file            file containing the data
  * @param _comment         string indicating that a line is a comment
  * @param _columnClass     indicates which column represents the class in the file
  * @param _nominal         array to know which classes are nominal
  * @param _originalData    data associated to the file (x)
  * @param _originalClasses classes associated to the file (y)
  * @author Néstor Rodríguez Vico
  */
class Data private[undersampling](private[undersampling] val _file: String, private[undersampling] val _comment: String,
                                  private[undersampling] val _columnClass: Int = -1, private[undersampling] val _nominal: Array[Int],
                                  private[undersampling] val _originalData: Array[Array[Any]],
                                  private[undersampling] val _originalClasses: Array[Any]) {


  //data obtained after applying an algorithm
  private[undersampling] var _resultData: Array[Array[Any]] = _
  // classes obtained after applying an algorithm
  private[undersampling] var _resultClasses: Array[Any] = _
  // index representing the kept elements
  private[undersampling] var _index: Array[Int] = _


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

  /** resultData getter
    *
    * @return read data
    */
  def resultData: Array[Array[Any]] = _resultData

  /** resultClasses getter
    *
    * @return read classes
    */
  def resultClasses: Array[Any] = _resultClasses

  /** index of kept elements getter
    *
    * @return read classes
    */
  def index: Array[Int] = _index
}
