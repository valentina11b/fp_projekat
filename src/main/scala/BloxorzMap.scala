import java.io.File
import scala.io.{BufferedSource, Source}

class BloxorzMap(var name: String, var map: Array[Array[Char]]) {

  def reverse(): Unit = {
    for ((line, i) <- map.zipWithIndex) {
      for ((c, j) <- line.zipWithIndex) {
        if (c == '.') {
          map(i)(j) = 'o'
        }
      }
    }
  }

  def invertStartEnd(): Unit = {
    var cnt = 0
    for ((line, i) <- map.zipWithIndex) {
      for ((c, j) <- line.zipWithIndex) {
        if (c == 'S') {
          map(i)(j) = 'T'
          cnt += 1
          if (cnt == 2)
            return
        } else if (c == 'T') {
          map(i)(j) = 'S'
          cnt += 1
          if (cnt == 2)
            return
        }
      }
    }
  }

  def replace(at: (Int, Int), withChar: Char): Unit = {
    map(at._1)(at._2) = withChar
  }

  def replaceStartEnd(start: Boolean, at: (Int, Int)): Unit = {
    var char = 'T'
    if (start) char = 'S'
    for ((line, i) <- map.zipWithIndex) {
      for ((c, j) <- line.zipWithIndex) {
        if (c == char) {
          map(i)(j) = 'o'
          map(at._1)(at._2) = char
          return
        }
      }
    }
  }

  def print() = {
    for (line <- map) {
      println(line.mkString("", "", ""))
    }
  }
}

object BloxorzMap {

  def loadMapAsCharMatrix(file: File): BloxorzMap = {
    val source = Source.fromFile(file)
    val lines = source.getLines().toArray
    source.close

    BloxorzMap(file.getName, lines.map(el => el.toCharArray))
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
    val lines = source.getLines().toArray
    source.close
    var startPosition = (0, 0)
    var endPosition = (0, 0)
    var allowedPositions = List[(Int, Int)]()
    var specialPositions = List[(Int, Int)]()

    for ((line, i) <- lines.zipWithIndex) {
      for ((c, j) <- line.zipWithIndex) {
        c match {
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
      }
    }
    Some(new Game(name, (startPosition, None), (endPosition, None), allowedPositions, specialPositions, lines))
  }

  def loadMapFromFile(file: File): Option[Game] = {
    loadMap(Source.fromFile(file), file.getName.replace(".txt", ""))
  }

  def loadMapFromFile(file: String): Option[Game] = {
    loadMap(Source.fromFile(file), file.replace(".txt", ""))
  }

}
