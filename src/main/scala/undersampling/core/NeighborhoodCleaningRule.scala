package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities.{Distances, nnRule}

import scala.collection.mutable.ArrayBuffer

/** Neighborhood Cleaning Rule
  *
  * @param data data to work with
  * @param seed seed to use. If it is not provided, it will use the system time
  * @author Néstor Rodríguez Vico
  */
class NeighborhoodCleaningRule(override private[undersampling] val data: Data,
                               override private[undersampling] val seed: Long = System.currentTimeMillis()) extends Algorithm(data, seed) {

  /** Compute the Neighborhood Cleaning Rule (NCL)
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @param k        number of neighbors to use when computing k-NN rule (normally 3 neighbors)
    * @return Data structure with all the important information and index of elements kept
    */
  def sample(file: Option[String] = None, distance: Distances.Distance, k: Int = 3): Data = {
    // Note: the notation used to refers the subsets of data is the original one.
    // Original paper: "Improving identification of difficult small classes by balancing class distribution" by J. Laurikkala.

    // index of the element of the interest class
    val indexC: Array[Int] = this.randomizedY.indices.toArray.filter((label: Int) => this.randomizedY(label) == this.untouchableClass)
    // index of the rest
    val indexO: Array[Int] = this.randomizedY.indices.toArray.diff(indexC.toList)

    // look for noisy elements in O
    val auxData: Data = new Data(_file = this.data._file, _comment = this.data._comment, _columnClass = this.data._columnClass,
      _nominal = this.data._nominal, _originalData = toXData(indexO map this.randomizedX), _originalClasses = indexO map this.randomizedY)
    val enn = new EditedNearestNeighbor(auxData)
    enn.setUntouchableClass(this.untouchableClass)
    val resultENN: Data = enn.sample(file = None, distance = Distances.EUCLIDEAN_NOMINAL, k = k)
    // noisy elements are the ones that are removed
    val indexA1: Array[Int] = this.randomizedY.indices.toList.diff(resultENN._index.toList).toArray

    val indexA2: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
    // get the size of all the classes
    val sizeOfClasses: Map[Any, Int] = this.randomizedY.groupBy(identity).mapValues((_: Array[Any]).length)
    val sizeC: Int = indexC.length

    // search for elements in O that misclassify elements in C
    for (index <- indexC) {
      // try to classify all the elements in C using O
      val label: (Any, Option[Array[Int]]) = nnRule(data = indexO map this.randomizedX, labels = indexO map this.randomizedY,
        newInstance = this.randomizedX(index), nominalValues = this.data._nominal, k = k, distance = distance, getIndex = true)

      // if is misclassified
      if (label._1 != this.randomizedY(index)) {
        // get the neighbours
        val neighbors: Array[Int] = label._2.get
        // and their classes
        val neighborsClasses: Array[Any] = neighbors.map((n: Int) => this.randomizedY(n))
        // and check if the size of theses classes is greater or equal than 0.5 * sizeC
        val shouldBeAdded: Array[Boolean] = neighborsClasses.collect { case c if sizeOfClasses(c) >= (0.5 * sizeC) => true }

        // add the neighbours that pass the test to indexA2
        for (pair <- neighbors zip shouldBeAdded) {
          if (pair._2) {
            indexA2 += pair._1
          }
        }
      }
    }

    // final index is allData - (indexA1 union indexA2)
    val finalIndex: Array[Int] = this.randomizedY.indices.diff(indexA1).diff(indexA2).toArray

    if (file.isDefined) {
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(this.normalizedData.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))

      // Recount of classes
      val newCounter: Array[(Any, Int)] = (finalIndex map this.randomizedY).groupBy(identity).mapValues((_: Array[Any]).length).toArray
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(finalIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (finalIndex.length.toFloat / this.randomizedX.length) * 100).toString)
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO", "NEW: %s".format(imbalancedRatio(newCounter)))
      // Save the log
      this.logger.storeFile(file.get + "_NCL")
    }

    this.data._resultData = (finalIndex map this.index).sorted map this.data._originalData
    this.data._resultClasses = (finalIndex map this.index).sorted map this.data._originalClasses
    this.data._index = (finalIndex map this.index).sorted

    this.data
  }
}
