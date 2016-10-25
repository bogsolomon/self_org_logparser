import java.io.{File, FileNotFoundException, IOException}

import scala.io.Source

class LogParser {

  def main(args: Array[String]): Unit = {
    getListOfSubDirectories(args(0)).foreach(parseFiles)
  }

  def parseFiles(path:String) : Unit = {
    val pathParts = path.split("-")
    val startServCount = pathParts(1).toInt
    val loadStatus = pathParts(2)

    getListOfFiles(path).foreach(processResults(_, startServCount, loadStatus))
  }

  def processResults(path:String, startServerCount:Int, loadStatus:String) : Unit = {
    try {
      for (line <- Source.fromFile(path).getLines()) {
        if (line.contains("Ant")) {

        }
      }
    } catch {
      case ex: FileNotFoundException => println("Couldn't find that file.")
      case ex: IOException => println("Had an IOException trying to read that file")
    }
  }

  def getListOfSubDirectories(directoryName: String): Array[String] = {
    new File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }

  def getListOfFiles(directoryName: String): Array[String] = {
    new File(directoryName).listFiles.filter(_.isFile).map(_.getName)
  }
}
