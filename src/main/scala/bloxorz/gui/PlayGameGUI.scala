package bloxorz.gui

import bloxorz.{FileDialog, FileUtil, Game}

import java.awt.Color
import javax.swing.Box
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.swing.event.{Key, KeyPressed}
import scala.swing.*

class Play(val game: Game) extends SimpleSwingApplication {

  def top: Frame = new Frame {
    title = "Bloxorz - " + game.name
    private val tiles: Vector[Tiles] = Tiles.createTilesVector(game)
    private val changed = mutable.ListBuffer[Tiles]()
    contents = new BoxPanel(Orientation.Vertical) {
      background = new Color(232, 232, 232)

      contents += new GridPanel(1, 1) {
        contents += Button("Load moves from file") {
          FileUtil.getPathOfFile(FileDialog.Read) match {
            case Some(path) =>
              loadMovesFromFile(path)
            case None =>
          }
        }
      }
      for ((line, i) <- game.map.zipWithIndex) {
        contents += new BoxPanel(Orientation.Horizontal) {
          background = new Color(232, 232, 232)
          for ((_, j) <- line.zipWithIndex) {
            contents += tiles(i * line.length + j)
            peer.add(Box.createHorizontalStrut(2))
          }
        }
        peer.add(Box.createVerticalStrut(2))
      }

      listenTo(keys)
      reactions += {
        case KeyPressed(_, key, _, _) if key == Key.Right || key == Key.Left || key == Key.Up || key == Key.Down
        =>
          makeMove(key match {
            case Key.Right => 'r'
            case Key.Left => 'l'
            case Key.Up => 'u'
            case Key.Down => 'd'
          }
          )
        case _ =>
      }
      focusable = true
      requestFocus()

    }

    def resetChanged(): Unit = {
      for (f <- changed) {
        f.reset()
      }
      changed.clear()
    }

    def makeMove(move: Char): Option[Boolean] = {
      val res = game.makeAMove(move)
      res match {
        case None =>
          println("Sorry, You lost!")
          resetChanged()
          game.currentPosition match {
            case (position, None) if game.coordinateInMap(position) =>
              val f = tiles(position._1 * game.N + position._2)
              f.changeColor(Color.RED)
              changed.append(f)
            case (position1, Some(position2)) =>
              if (game.coordinateInMap(position1)) {
                val f1 = tiles(position1._1 * game.N + position1._2)
                f1.changeColor(Color.RED)
                changed.append(f1)
              }
              if (game.coordinateInMap(position2)) {
                val f2 = tiles(position2._1 * game.N + position2._2)
                f2.changeColor(Color.RED)
                changed.append(f2)
              }
            case _ =>
          }
          val res = Dialog.showMessage(null, "Sorry, You lost!", title = "Game ended")
          resetChanged()
          game.reset()
        case Some(true) =>
          println("Congratulation, You won!")
          Dialog.showMessage(contents.head, "Congratulation, You won!", title = "Game ended")
          resetChanged()
          game.reset()

        case Some(false) =>
          resetChanged()
          game.currentPosition match {
            case (position, None) =>
              val f = tiles(position._1 * game.N + position._2)
              f.changeColor(Color.YELLOW)
              changed.append(f)
            case (position1, Some(position2)) =>
              val f1 = tiles(position1._1 * game.N + position1._2)
              f1.changeColor(Color.YELLOW)
              changed.append(f1)
              val f2 = tiles(position2._1 * game.N + position2._2)
              f2.changeColor(Color.YELLOW)
              changed.append(f2)
          }

      }
      res
    }

    private var closed = false

    override def closeOperation(): Unit = {
      closed = true
      super.closeOperation()
    }

    def loadMovesFromFile(file: String): Unit = {
      val source = Source.fromFile(file)
      val lines = source.getLines().toList
      source.close

      Future {

        @tailrec
        def readLine(lines: List[String]): Unit = {
          lines match {
            case Nil =>
              scala.swing.Swing.onEDTWait {
                contents.head.focusable = true
                contents.head.requestFocus()
              }
            case h :: t =>
              println(s"read move from line $h")
              if (h.trim.length != 1) {
                Dialog.showMessage(contents.head, s"Invalid line $h in file", title = "Invalid file")
                return
              }
              scala.swing.Swing.onEDTWait {
                makeMove(h.charAt(0)) match {
                  case None =>
                    contents.head.focusable = true
                    contents.head.requestFocus()
                    return
                  case _ =>
                }
              }

              if (!closed) {
                Thread.sleep(500)
                readLine(t)
              }
          }
        }

        readLine(lines)
      }
    }
  }
}
