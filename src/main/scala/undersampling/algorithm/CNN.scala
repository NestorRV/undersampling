package undersampling.algorithm

import smile.data.AttributeDataset
import undersampling.io.Logger
import undersampling.util.Utilities.{euclideanDistance, mode, toDataSet}

/** Implementation of the Condensed Nearest Neighbor decision rule (CNN rule).
  *
  * @param data data to work with.
  * @author Néstor Rodríguez Vico
  */
class CNN(private[undersampling] val data: AttributeDataset) {
  private[undersampling] val x: Array[Array[Double]] = data.toArray(new Array[Array[Double]](data.size))
  private[undersampling] val y: Array[Int] = data.toArray(new Array[Int](data.size))
  // 2 logs: one for info an another for data reduction process
  private[undersampling] val logger: Logger = new Logger(numberLogs = 2)
  this.logger.info += "DATA REDUCTION INFORMATION.\n"
  this.logger.info += "DATA SIZE REDUCTION INFORMATION. \nORIGINAL DATA SIZE: " + this.x.length.toString

  /** Decide the label of newInstance using the NNRule considering k neighbors of data set
    *
    * @param data        data where to search for
    * @param labels      labels asociated to each point in data
    * @param newInstance the point you want to clasify
    * @param k           number of neighbors to consider
    * @return the label associated to newPoint
    */
  def nnRule(data: Array[Array[Double]], labels: Array[Int], newInstance: Array[Double], k: Int): Int = {
    val distances: Array[(Double, Int)] = data.map((x: Array[Double]) => euclideanDistance(x, newInstance)).zipWithIndex.sortBy { case (d, _) => d }
    val bestDistances: Array[(Double, Int)] = distances.slice(0, if (k > data.length) data.length else k)
    val index: Array[Int] = bestDistances.map((d: (Double, Int)) => d._2)
    mode(index map labels)
  }

  /** Compute the CNN algorithm
    *
    * @param k    number of neighbors to consider when applying the NNRule
    * @param file file to store the log
    * @return
    */
  def compute(k: Int = 3, file: String): AttributeDataset = {
    // Indicate the corresponding group: 1 for store, 0 for unknown, -1 for grabbag
    val location: Array[Int] = List.fill(this.x.length)(0).toArray
    var iteration: Int = 0
    // The first element is added to store
    location(0) = 1
    this.logger.addMsg("\nIteration number: " + iteration + "\n", 0)
    this.logger.addMsg("Element " + 0 + " add to store for being the first.", 0)
    var changed = true

    // Iterate the data, x (except the first instance)
    for (element <- this.x.zipWithIndex.slice(1, this.x.length)) {
      // and classify each element with the actual content of store
      val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
      val label: Int = nnRule(data = index map this.x, labels = index map this.y, newInstance = element._1, k = k)
      // If is well classified
      if (label == this.y(element._2)) {
        // it is added to grabbag
        location(element._2) = -1
      } else {
        // otherwise, it is added to store
        location(element._2) = 1
        this.logger.addMsg("Element " + element._2 + " add to store for being incorrectly classified. " +
          "Original label: " + this.data.response().attribute().toString(this.y(element._2)) +
          ". Estimated label: " + this.data.response().attribute().toString(label), 0)
      }
    }

    this.logger.addMsg("Iteration " + iteration + ": grabbag size: " + location.count((z: Int) => z == -1) +
      ", store size: " + location.count((z: Int) => z == 1) + ", changed: " + changed + ".", 1)

    // After a first pass, iterate grabbag until is exhausted:
    // 1. There is no element in grabbag or
    // 2. There is no data change between grabbag and store after a full iteration
    while (location.count((z: Int) => z == -1) != 0 && changed) {
      iteration += 1
      this.logger.addMsg("\nIteration number: " + iteration + "\n", 0)
      changed = false
      // Now, instead of iterating x, we iterate grabbag
      for (element <- location.zipWithIndex.filter((x: (Int, Int)) => x._1 == -1)) {
        val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
        val label: Int = nnRule(data = index map this.x, labels = index map this.y, newInstance = this.x(element._2), k = k)
        if (label == this.y(element._2)) {
          location(element._2) = -1
        } else {
          location(element._2) = 1
          this.logger.addMsg("Element " + element._2 + " add to store for being incorrectly classified. " +
            "Original label: " + this.data.response().attribute().toString(this.y(element._2)) +
            ". Estimated label: " + this.data.response().attribute().toString(label), 0)
          changed = true
        }
      }

      this.logger.addMsg("Iteration " + iteration + ": grabbag size: " + location.count((z: Int) => z == -1) +
        ", store size: " + location.count((z: Int) => z == 1) + ", changed: " + changed + ".", 1)
    }

    // The final data is the content of store
    val storeIndex: Array[Int] = location.zipWithIndex.filter((x: (Int, Int)) => x._1 == 1).collect { case (_, a) => a }
    val store: Array[Array[Double]] = storeIndex map this.x
    val storeClasses: Array[Int] = storeIndex map this.y

    this.logger.info(1) += "\nNEW DATA SIZE: " + storeIndex.length + "\n"
    this.logger.info(1) += "\nREDUCTION PERCENTAGE: " + (100 - (storeIndex.length.toFloat / this.x.length) * 100) + "\n"
    this.logger.storeFile(file)

    toDataSet(this.data, store, storeClasses)
  }
}
