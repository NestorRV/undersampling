package undersampling.data

import scala.collection.mutable

/** Data structure used by the arff classes
  *
  * @param _file             file containing the data
  * @param _comment          string indicating that a line is a comment
  * @param _columnClass      indicates which column represents the class in the file
  * @param _nominal          array to know which classes are nominal
  * @param _originalData     data associated to the file (x)
  * @param _originalClasses  classes associated to the file (y)
  * @param _attributes       map with the form: index -> attributeName
  * @param _attributesValues map with the form attributeName -> type (it it's nominal, possible values instead of type)
  * @author Néstor Rodríguez Vico
  */
class ArffData private[undersampling](override private[undersampling] val _file: String, override private[undersampling] val _comment: String,
                                      override private[undersampling] val _columnClass: Int = -1, override private[undersampling] val _nominal: Array[Int],
                                      override private[undersampling] val _originalData: Array[Array[Any]],
                                      override private[undersampling] val _originalClasses: Array[Any], private[undersampling] val _relationName: String,
                                      private[undersampling] val _attributes: mutable.Map[Int, String],
                                      private[undersampling] val _attributesValues: mutable.Map[String, String]) extends Data(_file, _comment, _columnClass, _nominal, _originalData, _originalClasses) {
}
