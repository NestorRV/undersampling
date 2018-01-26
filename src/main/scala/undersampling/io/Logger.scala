package undersampling.io

import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Logger to collect info about undersampling process
  *
  * @param names names of the categories to log
  * @author Néstor Rodríguez Vico
  */
private[undersampling] class Logger(private[undersampling] val names: List[String]) {
  private[undersampling] val log: mutable.Map[String, ArrayBuffer[String]] = collection.mutable.Map[String, ArrayBuffer[String]]()

  names.foreach((name: String) => this.log(name) = new ArrayBuffer[String](0))

  /** Add a new message to the log
    *
    * @param category category where to add the line
    * @param msg      message to store
    */
  def addMsg(category: String, msg: String): Unit = {
    this.log(category) += msg
  }

  /** Add info to the info log
    *
    * @param category category to add to the log
    */
  def addCategory(category: String): Unit = {
    this.log(category) = new ArrayBuffer[String](0)
  }

  /** Store the logs into a file
    *
    * @param file filename where to store the logs
    */
  def storeFile(file: String): Unit = {
    val data = new PrintWriter(new File(file + ".log"))

    for ((k, v) <- this.log) {
      data.write(k + "\n")
      v.foreach((line: String) => data.write(line + "\n"))
      data.write("\n\n")
    }

    data.close()
  }
}
