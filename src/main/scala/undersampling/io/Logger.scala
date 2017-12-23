package undersampling.io

import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}

/** Logger to collect info about undersampling process
  *
  * @param numberLogs number of logs you want to use
  * @author Néstor Rodríguez Vico
  */
private[undersampling] class Logger(private[undersampling] val numberLogs: Int) {
  private[undersampling] val info: ArrayBuffer[String] = new ArrayBuffer[String](0)
  private[undersampling] val log: ArrayBuffer[ArrayBuffer[String]] = new ArrayBuffer[ArrayBuffer[String]]
  for (_ <- 0 until numberLogs)
    this.log += new ArrayBuffer[String](0)

  /** Add a new message to the log
    *
    * @param line message to store
    */
  def addMsg(line: String, logNumber: Int): Unit = {
    this.log(logNumber) += line
  }

  /** Add info to the info log
    *
    * @param info info to add to the log
    */
  def addInfo(info: String): Unit = {
    this.info += info
  }

  /** Store the logs into a file
    *
    * @param file filename where to store the logs
    */
  def storeFile(file: String): Unit = {
    val data = new PrintWriter(new File(file + ".log"))

    for (pair <- info zip log) {
      data.write(pair._1 + "\n")
      pair._2.foreach((line: String) => data.write(line + "\n"))
      data.write("\n\n")
    }

    data.close()
  }
}
