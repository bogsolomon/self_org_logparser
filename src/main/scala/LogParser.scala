import java.io.{File, FileNotFoundException, IOException}
import java.nio.file.{Files, Paths}

import scala.io.Source

class LogParser {

  def main(args: Array[String]): Unit = {
    getListOfSubDirectories(args(0)).foreach(parseFiles(_, args(0)))
  }

  def parseFiles(path:String, dirPath:String) : Unit = {
    val pathParts = path.split("-")
    val startServCount = pathParts(1).toInt
    val outDir = Paths.get(dirPath, path, "results")
    Files.createDirectory(outDir)

    var results = Map[Int, Map[String, List[String]]]()

    getListOfFiles(path).foreach({
      x => if (x.contains("control")) {
        val servCount = x.substring(7, x.length).toInt
        results = results + (servCount -> processResults(x))
      }
    })
  }

  def processResults(path:String) : Map[String, List[String]] = {
    var results = Map[String, List[String]]()
    try {
      var servAddress:String = ""
      for (line <- Source.fromFile(path).getLines()) {
        if (line.contains("physical address")) {
          servAddress = line.substring(line.indexOf("physical address") + "physical address".length, line.lastIndexOf(":"))
        }
        else if (line.contains("will jump to")) {
          val subLine = line.substring(line.indexOf("Ant UUID:"))
          val antUid = subLine.substring(subLine.indexOf("Ant UUID:")+ "Ant UUID:".length, subLine.lastIndexOf(","))
          val history = subLine.substring(subLine.indexOf("History:") + "History".length, subLine.indexOf(", Morph"))
          val historyVals = history.split(",")
          val value = historyVals.last.substring(historyVals.last.indexOf(",") + 1, historyVals.last.indexOf(")"))
          results = results + (antUid -> (value :: results.getOrElse(antUid, List[String]())))
        }
      }
    } catch {
      case ex: FileNotFoundException => println("Couldn't find that file.")
      case ex: IOException => println("Had an IOException trying to read that file")
    }
    results
  }

  def getListOfSubDirectories(directoryName: String): Array[String] = {
    new File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }

  def getListOfFiles(directoryName: String): Array[String] = {
    new File(directoryName).listFiles.filter(_.isFile).map(_.getName)
  }
}
