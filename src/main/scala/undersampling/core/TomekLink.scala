package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Tomek Link algorithm. Original paper: "Two Modifications of CNN" by Ivan Tomek.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class TomekLink(override private[undersampling] val data: Data,
                override private[undersampling] val seed: Long = System.currentTimeMillis(),
                override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** Undersampling method based in removing Tomek Links
    *
    * @param file     file to store the log. If its set to None, log process would not be done
    * @param distance distance to use when calling the NNRule algorithm
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance): Data = {
    // Start the time
    val initTime: Long = System.nanoTime()

    // Let's compute all the distances
    val distances: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]](0)
    if (this.data._nominal.length == 0) {
      for (instance <- this.randomizedX) {
        distances += this.randomizedX.map((ins: Array[Double]) => euclideanDistance(instance, ins))
      }
    } else {
      if (distance == Distances.EUCLIDEAN_NOMINAL) {
        for (instance <- this.randomizedX) {
          distances += this.randomizedX.map((ins: Array[Double]) => euclideanNominalDistance(instance, ins, this.data._nominal))
        }
      }
    }

    // Take the index of the elements that have a different class
    val candidates: mutable.Map[Any, Array[Int]] = collection.mutable.Map[Any, Array[Int]]()
    for (c <- this.randomizedY.distinct) {
      candidates += (c -> this.randomizedY.zipWithIndex.collect { case (a, b) if a != c => b })
    }

    // Look for the nearest neighbour in the rest of the classes
    val nearestNeighbor: Array[Int] = distances.zipWithIndex.map((row: (Array[Double], Int)) => row._1.indexOf((candidates(this.randomizedY(row._2)) map row._1).min)).toArray
    val tomekLinks: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)](0)
    // For each instance, I
    for (pair <- nearestNeighbor.zipWithIndex) {
      // If my nearest neighbour is J and the nearest neighbour of J it's me, I, I and J form a TomekLink
      if (nearestNeighbor(pair._1) == pair._2)
        tomekLinks += ((pair._1, pair._2))
    }

    // Instances that form a TomekLink are going to be removed
    val targetInstances: Array[Int] = tomekLinks.flatMap((x: (Int, Int)) => List(x._1, x._2)).toArray.distinct
    // We remove the all the instances except the associated with the untouchableClass
    val removedInstances: Array[Int] = targetInstances.zipWithIndex.collect { case (a, b) if a != this.untouchableClass => b }

    // Get the final index
    val finalIndex: Array[Int] = this.randomizedX.indices.diff(removedInstances).toArray

    // Stop the time
    val finishTime: Long = System.nanoTime()

    // Save the data
    this.data._resultData = (finalIndex map this.index).sorted map this.data._originalData
    this.data._resultClasses = (finalIndex map this.index).sorted map this.data._originalClasses
    this.data._index = (finalIndex map this.index).sorted

    if (file.isDefined) {
      this.logger.setNames(List("DATA SIZE REDUCTION INFORMATION", "IMBALANCED RATIO", "REDUCTION PERCENTAGE", "TIME"))
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "ORIGINAL SIZE: %d".format(this.normalizedData.length))
      this.logger.addMsg("IMBALANCED RATIO", "ORIGINAL: %s".format(imbalancedRatio(this.counter)))

      // Recount of classes
      val newCounter: Array[(Any, Int)] = (finalIndex map this.randomizedY).groupBy(identity).mapValues((_: Array[Any]).length).toArray
      this.logger.addMsg("DATA SIZE REDUCTION INFORMATION", "NEW DATA SIZE: %d".format(finalIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE", (100 - (finalIndex.length.toFloat / this.randomizedX.length) * 100).toString)
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
