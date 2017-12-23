package undersampling.algorithm

import smile.data.AttributeDataset
import undersampling.io.Logger
import undersampling.util.Utilities._

/** Implementation of the Condensed Nearest Neighbor decision rule (CNN rule).
  *
  * @param data data to work with.
  * @param seed seed to use. If it is not provided, it will use the system time
  * @author Néstor Rodríguez Vico
  */
class CNN(private[undersampling] val data: AttributeDataset, private[undersampling] val seed: Long = System.currentTimeMillis()) {
  // Shuffle the data to make it random
  private[undersampling] val random = new util.Random(seed)
  private[undersampling] val index: List[Int] = this.random.shuffle((0 until data.data().size()).toList)
  private[undersampling] val x: Array[Array[Double]] = (this.index map data.toArray(new Array[Array[Double]](data.size))).toArray
  private[undersampling] val y: Array[Int] = (this.index map data.toArray(new Array[Int](data.size))).toArray
  private[undersampling] val logger: Logger = new Logger(numberLogs = 1)
  this.logger.info += "DATA SIZE REDUCTION INFORMATION. \nORIGINAL DATA SIZE: " + this.x.length.toString

  /** Compute the CNN algorithm
    *
    * @param file file to store the log
    * @return
    */
  def compute(file: String): AttributeDataset = {
    // Indicate the corresponding group: 1 for store, 0 for unknown, -1 for grabbag
    val location: Array[Int] = List.fill(this.x.length)(0).toArray
    var iteration: Int = 0
    // The first element is added to store
    location(0) = 1
    var changed = true

    // Iterate the data, x (except the first instance)
    for (element <- this.x.zipWithIndex.slice(1, this.x.length)) {
      // and classify each element with the actual content of store
      val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
      val label: Int = nnRule(data = index map this.x, labels = index map this.y, newInstance = element._1, k = 1)
      // If is well classified
      if (label == this.y(element._2)) {
        // it is added to grabbag
        location(element._2) = -1
      } else {
        // otherwise, it is added to store
        location(element._2) = 1
      }
    }

    this.logger.addMsg("Iteration " + iteration + ": grabbag size: " + location.count((z: Int) => z == -1) +
      ", store size: " + location.count((z: Int) => z == 1) + ".", 0)

    // After a first pass, iterate grabbag until is exhausted:
    // 1. There is no element in grabbag or
    // 2. There is no data change between grabbag and store after a full iteration
    while (location.count((z: Int) => z == -1) != 0 && changed) {
      iteration += 1
      changed = false
      // Now, instead of iterating x, we iterate grabbag
      for (element <- location.zipWithIndex.filter((x: (Int, Int)) => x._1 == -1)) {
        val index: Array[Int] = location.zipWithIndex.collect { case (a, b) if a == 1 => b }
        val label: Int = nnRule(data = index map this.x, labels = index map this.y, newInstance = this.x(element._2), k = 1)
        if (label == this.y(element._2)) {
          location(element._2) = -1
        } else {
          location(element._2) = 1
          changed = true
        }
      }

      this.logger.addMsg("Iteration " + iteration + ": grabbag size: " + location.count((z: Int) => z == -1) +
        ", store size: " + location.count((z: Int) => z == 1) + ".", 0)
    }

    // The final data is the content of store
    val storeIndex: Array[Int] = location.zipWithIndex.filter((x: (Int, Int)) => x._1 == 1).collect { case (_, a) => a }
    val store: Array[Array[Double]] = storeIndex map this.x
    val storeClasses: Array[Int] = storeIndex map this.y

    this.logger.info(0) += "\nNEW DATA SIZE: " + storeIndex.length + "\n"
    this.logger.info(0) += "\nREDUCTION PERCENTAGE: " + (100 - (storeIndex.length.toFloat / this.x.length) * 100) + "\n"
    this.logger.storeFile(file)

    toDataSet(this.data, store, storeClasses)
  }
}
