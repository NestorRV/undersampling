package undersampling.util

import java.util.concurrent.TimeUnit

import undersampling.data.Data

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.math.{abs, pow, sqrt}

/** Set of utilities functions
  *
  * @author Néstor Rodríguez Vico
  */
object Utilities {

  /** Enumeration to store the possible distances
    *
    * EUCLIDEAN: Euclidean Distance for numeric values plus Nominal Distance (minimum distance, 0,
    * if the two elements are equal, maximum distance, 1, otherwise) for nominal values.
    *
    * HVDM: Proposed in "Improved Heterogeneous Distance Functions" by "D. Randall Wilson and Tony R. Martinez"
    *
    */
  object Distances extends Enumeration {
    type Distance = Value
    val EUCLIDEAN: Distances.Value = Value
    val HVDM: Distances.Value = Value
  }

  /** Compute the distances all elements against all elements
    *
    * @param data     elements to compute the distance
    * @param distance distance to use
    * @param nominal  array indicating the nominal elements, if present
    * @return matrix array with the distances
    */
  def computeDistances(data: Array[Array[Double]], distance: Distances.Distance, nominal: Array[Int], classes: Array[Any]): Array[Array[Double]] = {
    val attributesCounter: Array[Map[Double, Int]] = data.transpose.map((column: Array[Double]) => column.groupBy(identity).mapValues((_: Array[Double]).length))
    val attributesClassesCounter: Array[Map[Double, Map[Any, Int]]] = data.transpose.map((attribute: Array[Double]) => occurrencesByValueAndClass(attribute, classes))
    val sds: Array[Double] = data.transpose.map((column: Array[Double]) => standardDeviation(column))

    val distances: Array[Array[Double]] = Array.fill[Array[Double]](data.length)(new Array[Double](data.length))

    data.indices.par.foreach { i: Int =>
      data.indices.par.foreach { j: Int =>
        if (j >= i) {
          if (distance == Distances.HVDM) {
            distances(i)(j) = hvdm(data(i), data(j), nominal, sds, attributesCounter, attributesClassesCounter)
            distances(j)(i) = distances(i)(j)
          }
          else if (distance == Distances.EUCLIDEAN && nominal.length == 0) {
            distances(i)(j) = euclideanDistance(data(i), data(j))
            distances(j)(i) = distances(i)(j)
          }
          else {
            distances(i)(j) = euclideanNominalDistance(data(i), data(j), nominal)
            distances(j)(i) = distances(i)(j)
          }
        }
      }
    }

    distances
  }

  /** Compute the Euclidean Distance between two points
    *
    * @param xs first element
    * @param ys second element
    * @return Euclidean Distance between xs and ys
    */
  def euclideanDistance(xs: Array[Double], ys: Array[Double]): Double = {
    sqrt((xs zip ys).map((pair: (Double, Double)) => pow(pair._2 - pair._1, 2)).sum)
  }

  /** Compute the Euclidean-Nominal Distance between two points
    *
    * @param xs            first element
    * @param ys            second element
    * @param nominalValues array indicating the index of the nominal values
    * @return Euclidean-Nominal Distance between xs and ys
    */
  def euclideanNominalDistance(xs: Array[Double], ys: Array[Double], nominalValues: Array[Int]): Double = {
    sqrt((xs zip ys).zipWithIndex.map((element: ((Double, Double), Int)) =>
      if (nominalValues.contains(element._2)) if (element._1._2 == element._1._1) 0 else 1 else pow(element._1._2 - element._1._1, 2)).sum)
  }

  /** Compute the Heterogeneous Value Difference Metric Distance between two points
    *
    * @param xs            first element
    * @param ys            second element
    * @param nominalValues array indicating the index of the nominal values
    * @return Heterogeneous Value Difference Metric Distance between xs and ys
    */
  def hvdm(xs: Array[Double], ys: Array[Double], nominalValues: Array[Int], sds: Array[Double], attributesCounter: Array[Map[Double, Int]],
           attributesClassesCounter: Array[Map[Double, Map[Any, Int]]]): Double = {

    def normalized_diff(x: Double, y: Double, sd: Double): Double = abs(x - y) / (4 * sd)

    def normalized_vdm(nax: Double, nay: Double, naxClasses: Map[Any, Int], nayClasses: Map[Any, Int]): Double = {
      sqrt((naxClasses.values zip nayClasses.values).map((element: (Int, Int)) => pow(abs(element._1 / nax - element._2 / nay), 2)).sum)
    }

    (xs zip ys).zipWithIndex.map((element: ((Double, Double), Int)) =>
      if (nominalValues.contains(element._2))
        normalized_vdm(nax = attributesCounter(element._2)(element._1._1), nay = attributesCounter(element._2)(element._1._1),
          naxClasses = attributesClassesCounter(element._2)(element._1._1), nayClasses = attributesClassesCounter(element._2)(element._1._2)) else
        normalized_diff(element._1._1, element._1._2, sds(element._2))).sum
  }

  /** Compute the imbalanced ratio (number of instances of all the classes except the minority one divided by number of
    * instances of the minority class)
    *
    * @param counter Array containing a pair representing: (class, number of elements)
    * @return the imbalanced ratio
    */
  def imbalancedRatio(counter: Array[(Any, Int)]): Float = {
    val minorityElements: Int = counter.head._2

    (counter.map((_: (Any, Int))._2).sum.toFloat - minorityElements) / minorityElements
  }

  /** Compute KMeans algorithm
    *
    * @param data          data to be clustered
    * @param nominal       array to know which attributes are nominal
    * @param numClusters   number of clusters to be created
    * @param restarts      number of times to relaunch the algorithm
    * @param minDispersion stop if dispersion is lower than this value
    * @param maxIterations number of iterations to be done in KMeans algorithm
    * @param seed          seed to initialize the random object
    * @return (dispersion, centroids of the cluster, a map of the form: clusterID -> Array of elements in this cluster,
    *         a map of the form: elementID -> cluster associated)
    */
  def kMeans(data: Array[Array[Double]], nominal: Array[Int], numClusters: Int, restarts: Int, minDispersion: Double,
             maxIterations: Int, seed: Long): (Double, Array[Array[Double]], mutable.Map[Int, Array[Int]]) = {

    def run(centroids: Array[Array[Double]], minChangeInDispersion: Double, maxIterations: Int): (Double, Array[Array[Double]], mutable.Map[Int, Array[Int]]) = {

      def clusterIndex(data: Array[Array[Double]], centroids: Array[Array[Double]]): (Double, Array[Int]) = {
        val (distances, memberships) = data.par.map { element: Array[Double] =>
          val distances: Array[Double] = centroids.map((c: Array[Double]) => if (nominal.length == 0) euclideanDistance(c, element) else euclideanNominalDistance(c, element, nominal))
          val (bestDistance, bestCentroid) = distances.zipWithIndex.min
          (bestDistance * bestDistance, bestCentroid)
        }.toArray.unzip
        (distances.sum, memberships)
      }

      def getCentroids(data: Array[Array[Double]], memberships: Array[Int], numClusters: Int): (Array[Array[Double]],
        mutable.Map[Int, Array[Int]]) = {
        val assignment: mutable.Map[Int, ArrayBuffer[Int]] = mutable.LinkedHashMap[Int, ArrayBuffer[Int]]()
        (0 until numClusters).toList.foreach((c: Int) => assignment(c) = new ArrayBuffer[Int](0))

        for (index <- data.indices) {
          val clusterId = memberships(index)
          if (clusterId > -1)
            assignment(clusterId) += index
        }

        val centroids: Array[Array[Double]] = assignment.map((e: (Int, ArrayBuffer[Int])) => for (column <- (e._2.toArray map data).transpose) yield {
          column.sum / column.length
        }).toArray

        (centroids, assignment.map((element: (Int, ArrayBuffer[Int])) => element._1 -> element._2.toArray))
      }

      val numClusters: Int = centroids.length
      var iteration: Int = 0
      var lastDispersion: Double = Double.PositiveInfinity
      var dispersionDiff: Double = Double.PositiveInfinity
      var newCentroids: Array[Array[Double]] = centroids
      var assignment: mutable.Map[Int, Array[Int]] = mutable.LinkedHashMap[Int, Array[Int]]()

      while (iteration < maxIterations && dispersionDiff > minChangeInDispersion) {
        val (dispersion: Double, memberships: Array[Int]) = clusterIndex(data, newCentroids)
        val aux: (Array[Array[Double]], mutable.Map[Int, Array[Int]]) = getCentroids(data, memberships, numClusters)
        newCentroids = aux._1
        assignment = aux._2
        dispersionDiff = math.abs(lastDispersion - dispersion)
        lastDispersion = dispersion
        iteration += 1
      }
      (lastDispersion, newCentroids, assignment)
    }

    val centroids: Array[Array[Double]] = new scala.util.Random(seed).shuffle(data.indices.toList).toArray.slice(0, numClusters) map data
    val results: immutable.IndexedSeq[(Double, Array[Array[Double]], mutable.Map[Int, Array[Int]])] = (1 to restarts).map((_: Int) => run(centroids, minDispersion, maxIterations))
    val (bestDispersion, bestCentroids, bestAssignment) = results.minBy((_: (Double, Array[Array[Double]], mutable.Map[Int, Array[Int]]))._1)
    (bestDispersion, bestCentroids, bestAssignment)
  }

  /** Compute the mode of an array
    *
    * @param data array to compute the mode
    * @return the mode of the array
    */
  def mode(data: Array[Any]): Any = {
    data.groupBy(identity).mapValues((_: Array[Any]).length).toArray.maxBy((_: (Any, Int))._2)._2
  }

  /** Convert nanoseconds to minutes, seconds and milliseconds
    *
    * @param elapsedTime nanoseconds to be converted
    * @return String representing the conversion
    */
  def nanoTimeToString(elapsedTime: Long): String = {
    val minutes: Long = TimeUnit.NANOSECONDS.toMinutes(elapsedTime)
    val seconds: Long = TimeUnit.NANOSECONDS.toSeconds(elapsedTime) - TimeUnit.MINUTES.toSeconds(minutes)
    val millis: Long = TimeUnit.NANOSECONDS.toMillis(elapsedTime) - TimeUnit.MINUTES.toMillis(minutes) - TimeUnit.SECONDS.toMillis(seconds)
    "%03d min, %03d sec %03d millis".format(minutes, seconds, millis)
  }

  /** Decide the label using the NNRule considering k neighbors of data set
    *
    * @param distances        distances between the newInstance element and the rest of elements
    * @param selectedElements elements to consider
    * @param labels           labels associated to each point in data
    * @param k                number of neighbors to consider
    * @param which            if it's sets to "nearest", return the nearest which, if it sets "farthest", return the farthest which
    * @return the label associated to newPoint and the index of the k-nearest which, else None
    */
  def nnRule(distances: Array[Double], selectedElements: Array[Int], labels: Array[Any], k: Int, which: String = "nearest"): (Any, Array[Int]) = {
    val elements: Array[(Double, Int)] = if (which == "nearest")
      (selectedElements map distances.zipWithIndex).sortBy { case (d, _) => d } else
      (selectedElements map distances.zipWithIndex).sortBy { case (d, _) => d }.reverse
    val kBestNeighbours: Array[(Double, Int)] = elements.slice(0, if (k > selectedElements.length) selectedElements.length else k)
    val index: Array[Int] = kBestNeighbours.map((d: (Double, Int)) => d._2)
    (mode(index map labels), index)
  }

  /** Convert a data object into a matrix of doubles, taking care of missing values and nominal columns.
    * Missing data was treated using the most frequent value for nominal variables and the median for numeric variables.
    * Nominal columns are converted to doubles.
    *
    * @param data data to process
    * @return matrix of doubles containing the data
    */
  def processData(data: Data): Array[Array[Double]] = {
    val processedData: Array[Array[Double]] = data._originalData.transpose.zipWithIndex.map { column: (Array[Any], Int) =>
      // let's look for the NA values
      val naIndex: Array[Int] = column._1.zipWithIndex.filter((_: (Any, Int))._1 == "undersampling_NA").map((_: (Any, Int))._2)
      // If they exist
      if (naIndex.length != 0) {
        // Take the index of the elements that are not NA
        val nonNAIndex: Array[Int] = column._1.zipWithIndex.filter((_: (Any, Int))._1 != "undersampling_NA").map((_: (Any, Int))._2)
        // If the column is not a nominal value
        if (!data._nominal.contains(column._2)) {
          // compute the mean of the present values
          val arrayDouble: Array[Double] = (nonNAIndex map column._1).map((_: Any).asInstanceOf[Double])
          val mean: Double = arrayDouble.sum / arrayDouble.length
          val array: Array[Any] = column._1.clone()
          // replace all the NA values with the mean
          naIndex.foreach((index: Int) => array(index) = mean)

          array.map((_: Any).asInstanceOf[Double])
        } else {
          // compute the mode of the present values
          val m: Any = mode(nonNAIndex map column._1)
          val array: Array[Any] = column._1.clone()
          // replace all the NA values with the mode
          naIndex.foreach((index: Int) => array(index) = m)

          // After replacing the NA values, we change them to numerical values (0, 1, 2, ..., N)
          val uniqueValues: Array[Any] = array.distinct
          var counter: Double = -1.0
          val dict: Map[Any, Double] = uniqueValues.map { value: Any => counter += 1.0; value -> counter }.toMap
          array.indices.foreach((i: Int) => array(i) = dict(array(i)))

          array.map((_: Any).asInstanceOf[Double])
        }
      } else {
        // If there is no NA values
        // If the column is not a nominal value
        if (data._nominal.contains(column._2)) {
          val array: Array[Any] = column._1.clone()
          // we change them to numerical values (0, 1, 2, ..., N)
          val uniqueValues: Array[Any] = array.distinct
          var counter: Double = -1.0
          val dict: Map[Any, Double] = uniqueValues.map { value: Any => counter += 1.0; value -> counter }.toMap
          array.indices.foreach((i: Int) => array(i) = dict(array(i)))

          array.map((_: Any).asInstanceOf[Double])
        } else {
          // Store the data as is
          column._1.map((_: Any).asInstanceOf[Double])
        }
      }
    }

    processedData.transpose
  }

  /** Compute the number of occurrences for each value x for attribute represented by array attribute and output class c, for each class c in classes
    *
    * @param attribute attribute to be used
    * @param classes   classes present in the dataset
    * @return map of maps with the form: (value -> (class -> number of elements))
    */
  def occurrencesByValueAndClass(attribute: Array[Double], classes: Array[Any]): Map[Double, Map[Any, Int]] = {
    val auxMap: Map[Double, Array[Any]] = (attribute zip classes).groupBy((element: (Double, Any)) => element._1).map((element: (Double, Array[(Double, Any)])) => (element._1, element._2.map((value: (Double, Any)) => value._2)))
    auxMap.map((element: (Double, Array[Any])) => Map(element._1 -> element._2.groupBy(identity).mapValues((_: Array[Any]).length))).toList.flatten.toMap
  }

  /** Compute the standard deviation for an array
    *
    * @param xs array to be used
    * @return standard deviation of xs
    */
  def standardDeviation(xs: Array[Double]): Double = {
    val mean: Double = xs.sum / xs.length
    sqrt(xs.map((a: Double) => math.pow(a - mean, 2)).sum / xs.length)
  }

  /** Convert a double matrix to a matrix of Any
    *
    * @param d data to be converted
    * @return matrix of Any
    */
  def toXData(d: Array[Array[Double]]): Array[Array[Any]] = {
    d.map((row: Array[Double]) => row.map((_: Double).asInstanceOf[Any]))
  }

  /** Normalize the data as follow: for each column, x, (x-min(x))/(max(x)-min(x))
    * This method only normalize not nominal columns
    *
    * @return normalized data
    */
  def zeroOneNormalization(d: Data): Array[Array[Double]] = {
    val maxV: Array[Double] = d._processedData.transpose.map((col: Array[Double]) => col.max)
    val minV: Array[Double] = d._processedData.transpose.map((col: Array[Double]) => col.min)
    val result: Array[Array[Double]] = d._processedData.transpose.clone()

    d._processedData.transpose.indices.diff(d._nominal).par.foreach { index: Int =>
      val aux: Array[Double] = result(index).map((element: Double) => (element - minV(index)).toFloat / (maxV(index) - minV(index)))
      result(index) = if (aux.count((_: Double).isNaN) == 0) aux else Array.fill[Double](aux.length)(0.0)
    }
    result.transpose
  }
}