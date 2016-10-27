import java.io._
import java.nio.file.{Files, Path, Paths}
import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.time.format.DateTimeFormatter

import scala.io.Source

object LogParser {

  val formatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS");

  def main(args: Array[String]): Unit = {
    getListOfSubDirectories(args(0)).foreach(parseFiles(_, args(0)))
  }

  def parseFiles(path: String, dirPath: String): Unit = {
    val pathParts = path.split("-")
    val startServCount = pathParts(1).toInt
    val outDir = Paths.get(dirPath, path, "results")
    if (!Files.exists(outDir)) {
      Files.createDirectory(outDir)
    }

    var results = Map[Int, Map[String, List[String]]]()

    getListOfFiles(Paths.get(dirPath, path).toString).foreach({
      x => if (x.contains("control")) {
        val servCount = x.substring(7, x.length - 4).toInt
        results = results + (servCount -> processResults(dirPath, path, x))
      }
    })

    results.foreach(
      x => {
        val servCount = x._1
        x._2.foreach(
          y => {
            val ant = y._1
            val pathResults: Path = Paths.get(outDir.toString, "server-" + servCount + "-" + ant + ".out")
            if (Files.exists(pathResults)) {
              Files.delete(pathResults)
            }
            val file = Files.createFile(pathResults)
            val bw = new BufferedWriter(new FileWriter(file.toFile))
            y._2.foreach(
              z => {
                bw.write(z)
                bw.newLine()
              })
            bw.close()
          }
        )
      })
  }

  def processResults(dirPath: String, path: String, file: String): Map[String, List[String]] = {
    var results = Map[String, List[String]]()
    try {
      var servAddress: String = ""
      for (line <- Source.fromFile(Paths.get(dirPath, path, file).toString).getLines()) {
        if (line.contains("physical address")) {
          servAddress = line.substring(line.indexOf("physical address") + "physical address".length, line.lastIndexOf(":"))
        }
        else if (line.contains("will jump to")) {
          val timeStr = line.substring(0, line.indexOf(" "))
          val time = LocalTime.parse(timeStr, formatter)
          val subLine = line.substring(line.indexOf("Ant UUID:"))
          val antUid = subLine.substring(subLine.indexOf("Ant UUID:") + "Ant UUID:".length + 1, subLine.indexOf(","))
          val history = subLine.substring(subLine.indexOf("History:") + "History".length, subLine.indexOf(", Morph"))
          val historyVals = history.split(",")
          val localDateTime = LocalDateTime.of(LocalDate.now(), time)
          val value = localDateTime.toInstant(ZoneOffset.UTC).toEpochMilli + ", " +
            historyVals.last.substring(historyVals.last.indexOf(",") + 1, historyVals.last.indexOf(")"))
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
