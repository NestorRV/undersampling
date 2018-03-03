package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.collection.mutable.ArrayBuffer

/** Neighborhood Cleaning Rule. Original paper: "Improving identification of difficult small classes by balancing class distribution" by J. Laurikkala.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class NeighborhoodCleaningRule(override private[undersampling] val data: Data,
                               override private[undersampling] val seed: Long = System.currentTimeMillis(),
                               override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** Compute the Neighborhood Cleaning Rule (NCL)
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @param k        number of neighbors to use when computing k-NN rule (normally 3 neighbors)
    * @return Data structure with all the important information and index of elements kept
    */
  def sample(file: Option[String] = None, distance: Distances.Distance, k: Int = 3): Data = {
    // Note: the notation used to refers the subsets of data is the used in the original paper.

    // Use normalized data, but HVDM uses a special normalization
    // Use randomized data 
    val dataToWorkWith: Array[Array[Double]] = if (distance == Distances.EUCLIDEAN)
      (this.index map zeroOneNormalization(this.data)).toArray else
      (this.index map this.x).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Distances among the elements
    val distances: Array[Array[Double]] = computeDistances(dataToWorkWith, distance, this.data._nominal, this.y)

    // Start the time
    val initTime: Long = System.nanoTime()

    // index of the element of the interest class
    val indexC: Array[Int] = classesToWorkWith.indices.toArray.filter((label: Int) => classesToWorkWith(label) == this.untouchableClass)
    // index of the rest
    val indexO: Array[Int] = classesToWorkWith.indices.toArray.diff(indexC.toList)

    // look for noisy elements in O. Construct a Data object to be passed to EditedNearestNeighbor
    val auxData: Data = new Data(_nominal = this.data._nominal, _originalData = toXData(indexO map dataToWorkWith),
      _originalClasses = indexO map classesToWorkWith, _fileInfo = this.data._fileInfo)
    // But the untouchableClass must be the same
    val enn = new EditedNearestNeighbor(auxData, minorityClass = this.untouchableClass)
    val resultENN: Data = enn.sample(file = None, distance = distance, k = k)
    // noisy elements are the ones that are removed
    val indexA1: Array[Int] = classesToWorkWith.indices.toList.diff(resultENN._index.toList map indexO).toArray

    val indexA2: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
    // get the size of all the classes
    val sizeOfClasses: Map[Any, Int] = classesToWorkWith.groupBy(identity).mapValues((_: Array[Any]).length)
    val sizeC: Int = indexC.length

    // search for elements in O that misclassify elements in C
    for (index <- indexO) {
      // try to classify all the elements in C using O
      val label: (Any, Array[Int]) = nnRule(distances = distances(index), selectedElements = indexO.diff(List(index)), labels = classesToWorkWith, k = k)

      // if is misclassified
      if (label._1 != classesToWorkWith(index)) {
        // get the neighbours
        val neighbors: Array[Int] = label._2
        // and their classes
        val neighborsClasses: Array[Any] = neighbors.map((n: Int) => classesToWorkWith(n))
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
    val finalIndex: Array[Int] = classesToWorkWith.indices.diff(indexA1).diff(indexA2).toArray

    // Stop the time
    val finishTime: Long = System.nanoTime()

    this.data._resultData = (finalIndex map this.index).sorted map this.data._originalData
    this.data._resultClasses = (finalIndex map this.index).sorted map this.data._originalClasses
    this.data._index = (finalIndex map this.index).sorted

    if (file.isDefined) {
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE", "TIME"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(dataToWorkWith.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))

      // Recount of classes
      val newCounter: Array[(Any, Int)] = (finalIndex map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length).toArray
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(finalIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (finalIndex.length.toFloat / dataToWorkWith.length) * 100).toString)
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO", "NEW: %s".format(imbalancedRatio(newCounter)))
      // Save the time
      this.logger.addMsg("TIME", "Elapsed time: %s".format(nanoTimeToString(finishTime - initTime)))
      // Save the log
      this.logger.storeFile(file.get + "_NCL")
    }

    this.data
  }
}
