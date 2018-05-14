package undersampling.data

import undersampling.util.Utilities.processData

/** Data structure used by the algorithms
  *
  * @param _nominal         array to know which attributes are nominal
  * @param _originalData    data associated to the file (x)
  * @param _originalClasses classes associated to the file (y)
  * @param _fileInfo        object with the information needed to save the data into a file
  * @author Néstor Rodríguez Vico
  */
class Data private[undersampling](private[undersampling] val _nominal: Array[Int], private[undersampling] val _originalData: Array[Array[Any]],
                                  private[undersampling] val _originalClasses: Array[Any], private[undersampling] val _fileInfo: FileInfo) {

  // data without NA values and with nominal values transformed to numeric values
  private[undersampling] val _processedData: Array[Array[Double]] = processData(this)
  // data obtained after applying an algorithm
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
