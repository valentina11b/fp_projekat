
import java.awt.Color
import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.mutable
import scala.io.{BufferedSource, Source}

object FileUtil {

  def loadMaps(): List[Game] = {
    loadFiles() match {
      case None => List()
      case Some(listOfFiles) =>
        listOfFiles.map(f => {
          loadMapFromFile(f)
        }).filter(_.isDefined).map(_.get)
    }
  }

  def checkFileExtension(file: String, extension: String): Boolean = {
    var fileExtension = ""
    val i = file.lastIndexOf('.')
    if (i > 0) {
      fileExtension = file.substring(i + 1)
      fileExtension == extension
    } else {
      false
    }
  }

  def loadFiles(): Option[List[File]] = {
    val d = new File("src/main/maps")
    if (d.exists && d.isDirectory)
      Some(d.listFiles.filter(f => f.isFile && checkFileExtension(f.getName, "txt")).toList)
    else
      None
  }

  private def loadMap(source: BufferedSource, name: String): Option[Game] = {
    val lines = source.getLines().toVector
    source.close
    var startPosition = (0, 0)
    var endPosition = (0, 0)
    var allowedPositions = List[(Int, Int)]()
    var specialPositions = List[(Int, Int)]()

    val N = lines.map(line => line.length).max

    val map = for {(line, i) <- lines.zipWithIndex;
         newLine = (for {j <- 0 until N
                        char =
                          if (line.length - 1 < j) {
                            '-'
                          } else {
                            line(j) match {
                              case 'S' =>
                                startPosition = (i, j)
                                allowedPositions = (i, j) :: allowedPositions
                              case 'T' =>
                                endPosition = (i, j)
                                allowedPositions = (i, j) :: allowedPositions
                              case 'o' =>
                                allowedPositions = (i, j) :: allowedPositions
                              case '.' =>
                                specialPositions = (i, j) :: specialPositions
                              case '-' =>
                              case c =>
                                println(f"Invalid character '$c' at ($i, $j)")
                                return None
                            }
                            line(j)
                          }
                        } yield char).toVector
         }yield newLine;
    Some(new Game(name, (startPosition, None), (endPosition, None), allowedPositions, specialPositions, map))
  }

  def loadMapFromFile(file: File): Option[Game] = {
    loadMap(Source.fromFile(file), file.getName.replace(".txt", ""))
  }

  def loadMapFromFile(file: String): Option[Game] = {
    loadMap(Source.fromFile(file), file.replace(".txt", ""))
  }

  def checkMapPattern(fileName: String): Option[String] = {
    val source = Source.fromFile(fileName)
    val lines = source.getLines().toArray
    source.close
    for ((line, i) <- lines.zipWithIndex) {
      val Pattern = "([STo\\-.]+)".r
      line match {
        case Pattern(_) =>
        case _ =>
          return Some(f"Invalid character at line $i. : $line")
      }
    }
    None
  }
  
  def saveMapToFile(fileName: String, fields: Vector[Tiles]): Unit = {
    val path = "src/main/maps/" + fileName + "_edit_" + DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSSS").format(LocalDateTime.now) + ".txt"
    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path), StandardCharsets.UTF_8))
    var lastI = 0
    for (field <- fields) {

      if (lastI != field.i) {
        bw.newLine()
        lastI = field.i
      }

      field.color match {
        case Color.GRAY => bw.write('o')
        case Color.ORANGE => bw.write('.')
        case Color.WHITE => bw.write('-')
        case Color.YELLOW => bw.write('S')
        case Color.GREEN => bw.write('T')
        case _ =>
      }
    }
    bw.close()
  }

  def copyMapFromFile(fileName: String): Unit = {
    val from = java.nio.file.Paths.get(fileName)
    val to = java.nio.file.Paths.get("src/main/maps/" + from.getFileName.toString.replace(".txt", "") + "_" + DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSSS").format(LocalDateTime.now) + ".txt")

    java.nio.file.Files.copy(from, to, java.nio.file.StandardCopyOption.COPY_ATTRIBUTES)
  }

  def saveMovesToFile(fileName: String, moves: List[Char]): Unit = {
    val path = fileName + ".txt"
    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path), StandardCharsets.UTF_8))
    for (move <- moves) {
      bw.write(move + "\n")
    }
    bw.close()
  }

}
