package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

/** Tomek Link algorithm. Original paper: "Two Modifications of CNN" by Ivan Tomek.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class TL(override private[undersampling] val data: Data,
         override private[undersampling] val seed: Long = System.currentTimeMillis(),
         override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** Undersampling method based in removing Tomek Links
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance): Data = {
    // Use normalized data for EUCLIDEAN distance and randomized data
    val dataToWorkWith: Array[Array[Double]] = if (distance == Distances.EUCLIDEAN)
      (this.index map zeroOneNormalization(this.data)).toArray else
      (this.index map this.x).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Distances among the elements
    val distances: Array[Array[Double]] = computeDistances(dataToWorkWith, distance, this.data._nominal, this.y)

    // Start the time
    val initTime: Long = System.nanoTime()

    // Take the index of the elements that have a different class
    val candidates: Map[Any, Array[Int]] = classesToWorkWith.distinct.map { c: Any =>
      c -> classesToWorkWith.zipWithIndex.collect { case (a, b) if a != c => b }
    }.toMap

    // Look for the nearest neighbour in the rest of the classes
    val nearestNeighbor: Array[Int] = distances.zipWithIndex.map((row: (Array[Double], Int)) => row._1.indexOf((candidates(classesToWorkWith(row._2)) map row._1).min))

    // For each instance, I: If my nearest neighbour is J and the nearest neighbour of J it's me, I, I and J form a TL
    val tomekLinks: Array[(Int, Int)] = nearestNeighbor.zipWithIndex.filter((pair: (Int, Int)) => nearestNeighbor(pair._1) == pair._2)

    // Instances that form a TL are going to be removed
    val targetInstances: Array[Int] = tomekLinks.flatMap((x: (Int, Int)) => List(x._1, x._2)).distinct
    // We remove the all the instances except the associated with the untouchableClass
    val removedInstances: Array[Int] = targetInstances.zipWithIndex.collect { case (a, b) if a != this.untouchableClass => b }

    // Get the final index
    val finalIndex: Array[Int] = dataToWorkWith.indices.diff(removedInstances).toArray

    // Stop the time
    val finishTime: Long = System.nanoTime()

    // Save the data
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
      this.logger.storeFile(file.get + "_TL")
    }

    this.data
  }
}
