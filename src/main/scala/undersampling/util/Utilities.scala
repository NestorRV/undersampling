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

    val distances: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]](0)
    for (i <- data.indices) {
      val row = new Array[Double](data.length)
      for (j <- i until row.length) {
        if (distance == Distances.HVDM)
          row(j) = hvdm(data(i), data(j), nominal, sds, attributesCounter, attributesClassesCounter)
        else if (distance == Distances.EUCLIDEAN && nominal.length == 0)
          row(j) = euclideanDistance(data(i), data(j))
        else
          row(j) = euclideanNominalDistance(data(i), data(j), nominal)
      }
      distances += row
    }

    for (i <- data.indices) {
      for (j <- i until data.length) {
        distances(j)(i) = distances(i)(j)
      }
    }

    distances.toArray
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
    * @param numClusters   number of clusters to be created
    * @param restarts      number of times to relaunch the algorithm
    * @param minDispersion stop if dispersion is lower than this value
    * @param maxIterations number of iterations to be done in KMeans algorithm
    * @param seed          seed to initialize the random object
    * @return (dispersion, centroids of the cluster, a map of the form: clusterID -> Array of elements in this cluster,
    *         a map of the form: elementID -> cluster associated)
    */
  def kMeans(data: Array[Array[Double]], numClusters: Int, restarts: Int, minDispersion: Double,
             maxIterations: Int, seed: Long): (Double, Array[Array[Double]], mutable.Map[Int, ArrayBuffer[Int]]) = {

    def run(centroids: Array[Array[Double]], minChangeInDispersion: Double, maxIterations: Int): (Double, Array[Array[Double]], mutable.Map[Int, ArrayBuffer[Int]]) = {

      def clusterIndex(data: Array[Array[Double]], centroids: Array[Array[Double]]): (Double, Array[Int]) = {
        val (distances, memberships) = data.par.map { element: Array[Double] =>
          val distances: Array[Double] = centroids.map((c: Array[Double]) => euclideanDistance(c, element))
          val (bestDistance, bestCentroid) = distances.zipWithIndex.min
          (bestDistance * bestDistance, bestCentroid)
        }.toArray.unzip
        (distances.sum, memberships)
      }

      def getCentroids(data: Array[Array[Double]], memberships: Array[Int], numClusters: Int): (Array[Array[Double]],
        mutable.Map[Int, ArrayBuffer[Int]]) = {
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
        (centroids, assignment)
      }

      val numClusters: Int = centroids.length
      var iteration: Int = 0
      var lastDispersion: Double = Double.PositiveInfinity
      var dispersionDiff: Double = Double.PositiveInfinity
      var newCentroids: Array[Array[Double]] = centroids
      var assignment: mutable.Map[Int, ArrayBuffer[Int]] = mutable.LinkedHashMap[Int, ArrayBuffer[Int]]()

      while (iteration < maxIterations && dispersionDiff > minChangeInDispersion) {
        val (dispersion: Double, memberships: Array[Int]) = clusterIndex(data, newCentroids)
        val aux: (Array[Array[Double]], mutable.Map[Int, ArrayBuffer[Int]]) = getCentroids(data, memberships, numClusters)
        newCentroids = aux._1
        assignment = aux._2
        dispersionDiff = math.abs(lastDispersion - dispersion)
        lastDispersion = dispersion
        iteration += 1
      }
      (lastDispersion, newCentroids, assignment)
    }

    val centroids: Array[Array[Double]] = new scala.util.Random(seed).shuffle(data.indices.toList).toArray.slice(0, numClusters) map data
    val results: immutable.IndexedSeq[(Double, Array[Array[Double]], mutable.Map[Int, ArrayBuffer[Int]])] = (1 to restarts).map((_: Int) => run(centroids, minDispersion, maxIterations))
    val (bestDispersion, bestCentroids, bestAssignment) = results.minBy((_: (Double, Array[Array[Double]], mutable.Map[Int, ArrayBuffer[Int]]))._1)
    (bestDispersion, bestCentroids, bestAssignment)
  }

  /** Compute the mode of an array
    *
    * @param data array to compute the mode
    * @return the mode of the array
    */
  def mode(data: Array[Any]): Any = {
    val uniqueValues: Array[Any] = data.distinct
    val dict: mutable.Map[Any, Int] = collection.mutable.Map[Any, Int]()
    for (value <- uniqueValues) {
      dict += (value -> 0)
    }

    for (e <- data) {
      dict(e) += 1
    }

    dict.toSeq.sortBy((_: (Any, Int))._2).reverse.head._1
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
    * @return the label associated to newPoint and the index of the k-nearest neighbours, else None
    */
  def nnRule(distances: Array[Double], selectedElements: Array[Int], labels: Array[Any], k: Int): (Any, Array[Int]) = {
    val neighbours: Array[(Double, Int)] = (selectedElements map distances.zipWithIndex).sortBy { case (d, _) => d }
    val kBestNeighbours: Array[(Double, Int)] = neighbours.slice(0, if (k > selectedElements.length) selectedElements.length else k)
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
    val processedData: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]](0)

    for (column <- data._originalData.transpose.zipWithIndex) {
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
          val array: Array[Any] = column._1
          // replace all the NA values with the mean
          for (index <- naIndex)
            array(index) = mean

          processedData += array.map((_: Any).asInstanceOf[Double])
        } else {
          // compute the mode of the present values
          val m: Any = mode(nonNAIndex map column._1)
          val array: Array[Any] = column._1.clone()
          // replace all the NA values with the mode
          for (index <- naIndex)
            array(index) = m

          // After replacing the NA values, we change them to numerical values (0, 1, 2, ..., N)
          val uniqueValues: Array[Any] = array.distinct
          val dict: mutable.Map[Any, Double] = mutable.Map[Any, Double]()
          var counter: Double = 0.0
          for (value <- uniqueValues) {
            dict += (value -> counter)
            counter += 1.0
          }

          for (i <- array.indices) {
            array(i) = dict(array(i))
          }

          processedData += array.map((_: Any).asInstanceOf[Double])
        }
      } else {
        // If there is no NA values
        // If the column is not a nominal value
        if (data._nominal.contains(column._2)) {
          // we change them to numerical values (0, 1, 2, ..., N)
          val uniqueValues: Array[Any] = column._1.distinct
          val dict: mutable.Map[Any, Double] = mutable.Map[Any, Double]()
          var counter: Double = 0.0
          for (value <- uniqueValues) {
            dict += (value -> counter)
            counter += 1.0
          }

          val array: Array[Any] = column._1.clone()
          for (i <- array.indices) {
            array(i) = dict(array(i))
          }

          processedData += array.map((_: Any).asInstanceOf[Double])
        } else {
          // Store the data as is
          processedData += column._1.map((_: Any).asInstanceOf[Double])
        }
      }
    }

    processedData.toArray.transpose
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
    val r: Array[Array[Any]] = new Array[Array[Any]](d.length)
    for (row <- d.zipWithIndex) {
      r(row._2) = row._1.map((_: Double).asInstanceOf[Any])
    }

    r.asInstanceOf[Array[Array[Any]]]
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

    for (index <- d._processedData.transpose.indices.diff(d._nominal)) {
      val aux: Array[Double] = result(index).map((element: Double) => (element - minV(index)).toFloat / (maxV(index) - minV(index)))
      result(index) = if (aux.count((_: Double).isNaN) == 0) aux else Array.fill[Double](aux.length)(0.0)
    }
    result.transpose
  }
}