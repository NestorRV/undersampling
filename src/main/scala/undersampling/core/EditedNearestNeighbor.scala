package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities.{Distances, nnRule}

import scala.collection.mutable.ArrayBuffer

/** Edited Nearest Neighbor rule
  *
  * @param data data to work with
  * @param seed seed to use. If it is not provided, it will use the system time
  * @author Néstor Rodríguez Vico
  */
class EditedNearestNeighbor(override private[undersampling] val data: Data,
                            override private[undersampling] val seed: Long = System.currentTimeMillis()) extends Algorithm(data, seed) {


  /** Compute the Edited Nearest Neighbor rule (ENN rule)
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @param k        number of neighbors to use when computing k-NN rule (normally 3 neighbors)
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance, k: Int = 3): Data = {
    // Original paper: "Asymptotic Properties of Nearest Neighbor Rules Using Edited Data" by Dennis L. Wilson.

    val selectedElements: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
    val indices: Array[Int] = this.randomizedY.indices.toArray

    for (index <- indices) {
      // don't process the element if it is from the untouchableClass
      if (this.randomizedY(index) != this.untouchableClass) {
        // indices.diff(List(index)) is to exclude the actual element -> LeaveOneOut
        val label: (Any, Option[Array[Int]]) = nnRule(data = indices.diff(List(index)) map this.randomizedX,
          labels = indices.diff(List(index)) map this.randomizedY, newInstance = this.randomizedX(index),
          nominalValues = this.data._nominal, k = k, distance = distance)

        // if the label matches (it is well classified)
        if (label._1 == this.randomizedY(index)) {
          // the element is useful
          selectedElements += index
        }
      }
    }

    if (file.isDefined) {
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(this.normalizedData.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))

      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(this.normalizedData.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))
      // Recount of classes
      val newCounter: Array[(Any, Int)] = (selectedElements.toArray map this.randomizedY).groupBy(identity).mapValues((_: Array[Any]).length).toArray
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(selectedElements.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (selectedElements.length.toFloat / this.randomizedX.length) * 100).toString)
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO", "NEW: %s".format(imbalancedRatio(newCounter)))
      // Save the log
      this.logger.storeFile(file.get + "_ENN")
    }

    this.data._resultData = (selectedElements.toArray map this.index).sorted map this.data._originalData
    this.data._resultClasses = (selectedElements.toArray map this.index).sorted map this.data._originalClasses
    this.data._index = (selectedElements.toArray map this.index).sorted

    this.data
  }
}
