import java.io._
import java.nio.file.{Files, Path, Paths}
import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.time.format.DateTimeFormatter

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object LogParser {

  val formatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS");

  def main(args: Array[String]): Unit = {
    getListOfSubDirectories(args(0)).foreach(parseFiles(_, args(0)))
  }

  def parseFiles(path: String, dirPath: String): Unit = {
    if (path.equals("results")) {
      return
    }
    val pathParts = path.split("-")
    val startServCount = pathParts(1).toInt

    var results = Map[Int, Map[String, List[String]]]()

    getListOfFiles(Paths.get(dirPath, path).toString).foreach({
      x => if (x.contains("control")) {
        val servCount = x.substring(7, x.length - 4).toInt
        results = results + (servCount -> processResults(dirPath, path, x))
      } else if (x.contains("manager")) {

      }
    })

    val outPathResults: Path = Paths.get(dirPath.toString, "results", path)
    if (!Files.exists(outPathResults)) {
      Files.createDirectories(outPathResults)
    }

    var resultsAnt = Map[String, List[String]]()

    results.foreach(
      x => {
        val servCount = x._1
        x._2.foreach(
          y => {
            val ant = y._1
            resultsAnt = resultsAnt + (ant -> resultsAnt.getOrElse(ant, List[String]()))
            y._2.foreach(
              z => {
                val timestamp = z.substring(0, z.indexOf(","))
                var antServResult = resultsAnt.get(ant).get
                antServResult = (timestamp + "," + servCount) :: antServResult
                resultsAnt = resultsAnt + (ant -> antServResult)
              }
            )
          }
        )
      })

    resultsAnt.foreach(
      x => {
        val ant = x._1
        x._2.foreach(
          y => {
            var antServResult = resultsAnt.get(ant).get
            antServResult = antServResult.sorted
            resultsAnt = resultsAnt + (ant -> antServResult)
          }
        )
      }
    )

    results.foreach(
      x => {
        val servCount = x._1
        x._2.foreach(
          y => {
            val ant = y._1
            val pathResults: Path = Paths.get(outPathResults.toString, "server-" + servCount + "-" + ant + ".csv")
            if (Files.exists(pathResults)) {
              Files.delete(pathResults)
            }
            val file = Files.createFile(pathResults)
            val bw = new BufferedWriter(new FileWriter(file.toFile))
            if (ant.equals("perf")) {
              bw.write("Time,CPU,ClientsInRoom,Rooms,Users,LocalUsers,Latency,BwUp,BwDown,InStreamBw," +
                "OutStreamBw,Streams,OutStreams")
              bw.newLine()
            }
            else {
              bw.write("Time,Pheromone")
              bw.newLine()
            }
            y._2.foreach(
              z => {
                bw.write(z)
                bw.newLine()
              })
            bw.close()
          }
        )
      })

    resultsAnt.foreach(
      x => {
        val ant = x._1
        if (!ant.equals("perf") && !ant.equals("control")) {
          val pathResults: Path = Paths.get(outPathResults.toString, "ant-" + ant + ".csv")
          if (Files.exists(pathResults)) {
            Files.delete(pathResults)
          }
          val file = Files.createFile(pathResults)
          val bw = new BufferedWriter(new FileWriter(file.toFile))
          bw.write("Time,Server")
          bw.newLine()
          x._2.foreach(
            y => {
              bw.write(y)
              bw.newLine()
            })
          bw.close()
        }
      }
    )
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
        } else if (line.contains("Decaying")) {
          val timeStr = line.substring(0, line.indexOf(" "))
          val time = LocalTime.parse(timeStr, formatter)
          val localDateTime = LocalDateTime.of(LocalDate.now(), time)
          val uid = "control"
          val value = localDateTime.toInstant(ZoneOffset.UTC).toEpochMilli + ", " +
            line.substring(line.indexOf("pheromone") + "pheromone".length + 1, line.length)
          results = results + (uid -> (value :: results.getOrElse(uid, List[String]())))
        } else if (line.contains("Red5Sensor") && !line.contains("Couldn't get sensor metrics")) {
          val timeStr = line.substring(0, line.indexOf(" "))
          val time = LocalTime.parse(timeStr, formatter)
          val localDateTime = LocalDateTime.of(LocalDate.now(), time)
          val subline = line.substring(line.indexOf("SensorMeasurement"))
          val values = subline.split(",")
          var valuesOut = new ArrayBuffer[String]()
          valuesOut += values(0).substring(values(0).indexOf("(") + 1)
          valuesOut += values(1).substring(values(1).indexOf("(") + 1)
          valuesOut += values(2).substring(0, values(2).size - 1)
          valuesOut += values(3).substring(values(3).indexOf("(") + 1)
          valuesOut += values(4).substring(0, values(4).size - 1)
          valuesOut += values(5).substring(values(5).indexOf("(") + 1)
          valuesOut += values(6)
          valuesOut += values(7).substring(0, values(7).size - 1)
          valuesOut += values(8).substring(values(8).indexOf("(") + 1)
          valuesOut += values(9)
          valuesOut += values(10)
          valuesOut += values(11).substring(0, values(11).size - 2)
          val value = localDateTime.toInstant(ZoneOffset.UTC).toEpochMilli + "," +
            valuesOut.mkString(",")
          results = results + ("perf" -> (value :: results.getOrElse("perf", List[String]())))
        }
      }
    } catch {
      case ex: FileNotFoundException => println("Couldn't find that file.")
      case ex: IOException => println("Had an IOException trying to read that file")
    }
    for ((x,y) <- results) results + x -> y.reverse
    results
  }

  def getListOfSubDirectories(directoryName: String): Array[String] = {
    new File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }

  def getListOfFiles(directoryName: String): Array[String] = {
    new File(directoryName).listFiles.filter(_.isFile).map(_.getName)
  }
}
