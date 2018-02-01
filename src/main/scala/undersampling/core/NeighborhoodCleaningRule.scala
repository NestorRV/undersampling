package undersampling.core

import undersampling.util.Utilities.{Distances, nnRule}

import scala.collection.mutable.ArrayBuffer

/** Neighborhood Cleaning Rule
  *
  * @param x             data to work with
  * @param y             labels of the data associated with x
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass integer representing the class to keep it untouched
  * @author Néstor Rodríguez Vico
  */
class NeighborhoodCleaningRule(override private[undersampling] val x: Array[Array[Double]],
                               override private[undersampling] val y: Array[Int],
                               override private[undersampling] val seed: Long = System.currentTimeMillis(),
                               minorityClass: Int = 0) extends Algorithm(x, y, seed, minorityClass) {

  /** Compute the Neighborhood Cleaning Rule (NCL)
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @param k        number of neighbors to use when computing k-NN rule (normally 3 neighbors)
    * @return reduced data, reduced labels, index of elements kept
    */
  def sample(file: Option[String] = None, distance: Distances.Distance, k: Int = 3): (Array[Array[Double]], Array[Int], Array[Int]) = {
    // Note: the notation used to refers the subsets of data is the original one.
    // Original paper: "Improving identification of difficult small classes by balancing class distribution" by J. Laurikkala.

    // index of the element of the interest class
    val indexC: Array[Int] = this.randomizedY.indices.toArray.filter((label: Int) => this.randomizedY(label) == this.untouchableClass)
    // index of the rest
    val indexO: Array[Int] = this.randomizedY.indices.toArray.diff(indexC.toList)

    // look for noisy elements in O
    val enn = new EditedNearestNeighbor(indexO map this.randomizedX, indexO map this.randomizedY)
    val resultENN: (Array[Array[Double]], Array[Int], Array[Int]) = enn.sample(file = None, distance = Distances.EUCLIDEAN, k = k)
    // noisy elements are the ones that are removed
    val indexA1: Array[Int] = this.randomizedY.indices.toList.diff(resultENN._3.toList).toArray

    val indexA2: ArrayBuffer[Int] = new ArrayBuffer[Int](0)
    // get the size of all the classes
    val sizeOfClasses: Array[Int] = this.randomizedY.groupBy(identity).mapValues((_: Array[Int]).length).toArray.sortBy((count: (Int, Int)) => count._1).collect { case (_, s) => s }
    val sizeC: Int = indexC.length

    // search for elements in O that misclassify elements in C
    for (index <- indexC) {
      // try to classify all the elements in C using O
      val label: (Int, Option[Array[Int]]) = nnRule(data = indexO map this.randomizedX, labels = indexO map this.randomizedY,
        newInstance = this.randomizedX(index), newInstanceLabel = this.randomizedY(index), k = k, distance = distance, getIndex = true)

      // if is misclassified
      if (label._1 != this.randomizedY(index)) {
        // get the neighbours
        val neighbors: Array[Int] = label._2.get
        // and their classes
        val neighborsClasses: Array[Int] = neighbors.map((n: Int) => this.randomizedY(n))
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
      val newCounter: Array[(Int, Int)] = (finalIndex map this.randomizedY).groupBy(identity).mapValues((_: Array[Int]).length).toArray
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(finalIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (finalIndex.length.toFloat / this.randomizedX.length) * 100).toString)
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO", "NEW: %s".format(imbalancedRatio(newCounter)))
      // Save the log
      this.logger.storeFile(file.get + "_NCL")
    }

    (denormalizedData(finalIndex map this.randomizedX), finalIndex map this.randomizedY, finalIndex)
  }
}
