package bloxorz

import bloxorz.gui.Tiles

import java.awt.Color
import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.{JFileChooser, JFrame}
import scala.collection.mutable
import scala.io.{BufferedSource, Source}


abstract class FileDialog(val title: String) {}

object FileDialog {
  case object Read extends FileDialog("Choose file")

  case object Write extends FileDialog("Save file")
}

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
    file.lastIndexOf('.') > 0 && file.substring(file.lastIndexOf('.') + 1) == extension
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
                   } yield newLine;
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
    var numOfS = 0
    var numOfT = 0
    for ((line, i) <- lines.zipWithIndex) {
      if (line.contains("S")) {
        numOfS += 1
        if (line.split("S").length > 2) return Some("There can't be more than one start position")
      }
      if (line.contains("T")) {
        numOfT += 1
        if (line.split("T").length > 2) return Some("There can't be more than one end position")
      }
      val Pattern = "([STo\\-.]+)".r
      line match {
        case Pattern(_) =>
        case _ =>
          return Some(f"Invalid character at line $i. : $line")
      }
    }
    if (numOfS != 1)
      return Some("There must be one start position")
    if (numOfT != 1)
      return Some("There must be one end position")
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


  def getPathOfFile(to: FileDialog): Option[String] = {
    val chooser = new JFileChooser(".")
    val filter = new FileNameExtensionFilter("Text Files", "txt")
    chooser.setFileFilter(filter)
    to match {
      case FileDialog.Read =>
        chooser.showOpenDialog(new JFrame())
      case FileDialog.Write =>
        chooser.showSaveDialog(new JFrame())
    }
    chooser.setDialogTitle(to.title)
    val file = chooser.getSelectedFile
    if (file != null) {
      return Some(file.getAbsolutePath)
    }
    None
  }

}
