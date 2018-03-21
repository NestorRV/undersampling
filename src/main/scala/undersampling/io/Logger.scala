package undersampling.io

import java.io.{File, PrintWriter}

import scala.collection.mutable.ArrayBuffer

/** Logger to collect info about undersampling process
  *
  * @author Néstor Rodríguez Vico
  */
private[undersampling] class Logger {
  private[undersampling] val log: ArrayBuffer[String] = new ArrayBuffer[String](0)

  /** Add a new message to the log
    *
    * @param msg message to store
    */
  def addMsg(msg: String): Unit = {
    this.log += msg
  }

  /** Store the logs into a file
    *
    * @param file filename where to store the logs
    */
  def storeFile(file: String): Unit = {
    val data = new PrintWriter(new File(file + ".log"))
    this.log.foreach((line: String) => data.write(line + "\n"))
    data.close()
  }
}
