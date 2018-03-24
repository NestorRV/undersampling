package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

/** ClusterOSS. Original paper: "ClusterOSS: a new undersampling method for imbalanced learning."
  * by Victor H Barella, Eduardo P Costa and André C P L F Carvalho.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class ClusterOSS(override private[undersampling] val data: Data,
                 override private[undersampling] val seed: Long = System.currentTimeMillis(),
                 override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** Undersampling method based in ClusterOSS
    *
    * @param file          file to store the log. If its set to None, log process would not be done
    * @param distance      distance to use when calling the NNRule algorithm
    * @param k             number of neighbours to use when computing k-NN rule (normally 3 neighbours)
    * @param numClusters   number of clusters to be created by KMeans algorithm
    * @param restarts      number of times to relaunch KMeans algorithm
    * @param minDispersion stop KMeans algorithm if dispersion is lower than this value
    * @param maxIterations number of iterations to be done in KMeans algorithm
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, distance: Distances.Distance, k: Int = 3, numClusters: Int,
             restarts: Int = 3, minDispersion: Double = 0.0001, maxIterations: Int = 200): Data = {

    // Use randomized data 
    val dataToWorkWith: Array[Array[Double]] = (this.index map this.x).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Start the time
    val initTime: Long = System.nanoTime()

    val initDistancesTime: Long = System.nanoTime()
    // Distances among the elements
    val distances: Array[Array[Double]] = computeDistances(dataToWorkWith, distance, this.data._nominal, this.y)
    val distancesTime: Long = System.nanoTime() - initDistancesTime

    val majElements: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (label, i) if label != this.untouchableClass => i }
    val minElements: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (label, i) if label == this.untouchableClass => i }

    val (_, centroids, assignment) = kMeans(data = majElements map dataToWorkWith, nominal = this.data._nominal, numClusters = numClusters, restarts = restarts,
      minDispersion = minDispersion, maxIterations = maxIterations, seed = this.seed)

    val kMeansTime: Long = System.nanoTime() - initTime

    val result: (Array[Int], Array[Array[Int]]) = assignment.par.map { cluster: (Int, Array[Int]) =>
      val distances: Array[(Int, Double)] = cluster._2.map { instance: Int =>
        if (this.data._nominal.length == 0)
          (instance, euclideanDistance(dataToWorkWith(instance), centroids(cluster._1)))
        else
          (instance, euclideanNominalDistance(dataToWorkWith(instance), centroids(cluster._1), this.data._nominal))
      }

      val closestInstance: Int = if (distances.isEmpty) -1 else distances.minBy((_: (Int, Double))._2)._1
      (closestInstance, cluster._2.diff(List(closestInstance)))
    }.toArray.unzip

    // Remove foo values
    val train: Array[Int] = result._1.diff(List(-1))
    // Flatten all the clusters
    val test: Array[Int] = result._2.flatten

    val calculatedLabels: Array[(Int, (Any, Array[Int]))] = test.map { testInstance: Int =>
      (testInstance, nnRule(distances = distances(testInstance), selectedElements = train, labels = classesToWorkWith, k = 1))
    }

    // if the label matches (it is well classified) the element is useful
    val misclassified: Array[Int] = calculatedLabels.collect { case (i, (label, _)) if label != classesToWorkWith(i) => i }

    val newData: Array[Int] = misclassified ++ train

    // Construct a Data object to be passed to Tomek Link
    val auxData: Data = new Data(_nominal = this.data._nominal, _originalData = toXData(newData map dataToWorkWith),
      _originalClasses = newData map classesToWorkWith, _fileInfo = this.data._fileInfo)
    // But the untouchableClass must be the same
    val tl = new TomekLink(auxData, minorityClass = this.untouchableClass)
    val resultTL: Data = tl.sample(file = None, distance = distance)
    // The final index is the result of applying TomekLink to the content of newData
    val finalIndex: Array[Int] = ((resultTL._index.toList map newData) ++ minElements).toArray

    // Stop the time
    val finishTime: Long = System.nanoTime()

    this.data._resultData = (finalIndex map this.index).sorted map this.data._originalData
    this.data._resultClasses = (finalIndex map this.index).sorted map this.data._originalClasses
    this.data._index = (finalIndex map this.index).sorted

    if (file.isDefined) {
      // Recount of classes
      val newCounter: Map[Any, Int] = (finalIndex map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length)

      this.logger.addMsg("ORIGINAL SIZE: %d".format(dataToWorkWith.length))
      this.logger.addMsg("NEW DATA SIZE: %d".format(finalIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE: %s".format(100 - (finalIndex.length.toFloat / dataToWorkWith.length) * 100))

      this.logger.addMsg("ORIGINAL IMBALANCED RATIO: %s".format(imbalancedRatio(this.counter, this.untouchableClass)))
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO: %s".format(imbalancedRatio(newCounter, this.untouchableClass)))

      // Save the kmeans time
      this.logger.addMsg("KMEANS CALCULATION TIME: %s".format(nanoTimeToString(kMeansTime)))
      // Save the distance calculation time
      this.logger.addMsg("DISTANCES CALCULATION TIME: %s".format(nanoTimeToString(distancesTime)))
      // Save the time
      this.logger.addMsg("TOTAL ELAPSED TIME: %s".format(nanoTimeToString(finishTime - initTime)))

      // Save the log
      this.logger.storeFile(file.get + "_ClusterOSS")
    }

    this.data
  }
}