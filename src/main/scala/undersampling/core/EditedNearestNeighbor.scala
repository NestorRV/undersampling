package undersampling.core

import undersampling.util.Utilities.{Distances, nnRule}

/** Edited Nearest Neighbor rule
  *
  * @param x             data to work with
  * @param y             labels of the data associated with x
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass integer representing the class to keep it untouched
  * @author Néstor Rodríguez Vico
  */
class EditedNearestNeighbor(override private[undersampling] val x: Array[Array[Double]],
                            override private[undersampling] val y: Array[Int],
                            override private[undersampling] val seed: Long = System.currentTimeMillis(),
                            minorityClass: Int = 0) extends Algorithm(x, y, seed, minorityClass) {


  /** Compute the Edited Nearest Neighbor rule (ENN rule)
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @param k        number of neighbors to use when computing k-NN rule (normally 3 neighbors)
    * @return reduced data, reduced labels, index of elements kept
    */
  def sample(file: Option[String] = None, distance: Distances.Distance, k: Int = 3): (Array[Array[Double]], Array[Int], Array[Int]) = {
    if (file.isDefined) {
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(this.normalizedData.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))
    }

    var selectedElements: Array[Int] = this.randomizedY.indices.toArray
    val indices: Array[Int] = this.randomizedY.indices.toArray

    for (index <- indices) {
      // don't process the element if it is from the untouchableClass
      if (this.randomizedY(index) != this.untouchableClass) {
        // selectedElements.diff(List(index)) is to exclude the actual element -> LeaveOneOut
        val label: Int = nnRule(data = selectedElements.diff(List(index)) map this.randomizedX,
          labels = selectedElements.diff(List(index)) map this.randomizedY, newInstance = this.randomizedX(index),
          newInstanceLabel = this.randomizedY(index), k = k, distance = distance)

        // if the label does not match
        if (label != this.randomizedY(index)) {
          // remove it
          selectedElements = selectedElements.diff(List(index))
        }
      }
    }

    if (file.isDefined) {
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(this.normalizedData.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))
      // Recount of classes
      val newCounter: Array[(Int, Int)] = (selectedElements map this.randomizedY).groupBy((l: Int) => l).map((t: (Int, Array[Int])) => (t._1, t._2.length)).toArray
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(selectedElements.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (selectedElements.length.toFloat / this.randomizedX.length) * 100).toString)
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO", "NEW: %s".format(imbalancedRatio(newCounter)))
      // Save the logk
      this.logger.storeFile(file.get + "_ENN")
    }

    (selectedElements map this.randomizedX, selectedElements map this.randomizedY, selectedElements)
  }
}
