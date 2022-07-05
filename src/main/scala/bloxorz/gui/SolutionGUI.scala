package bloxorz.gui

import bloxorz.{FileDialog, FileUtil, Game}

import java.awt.Color
import javax.swing.Box
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.swing.*

class ShowSolution(val game: Game, val solution: Option[List[Char]]) extends SimpleSwingApplication {

  def top: Frame = new Frame {

    private var closed = false

    override def closeOperation(): Unit = {
      closed = true
      super.closeOperation()
    }

    def start(): Unit = {
      game.reset()
      solution match {
        case None =>
          Dialog.showMessage(contents.head, "Game has no solution", title = "Solver")
        case Some(solution) =>
          Future {
            solution.foreach(move =>
              scala.swing.Swing.onEDTWait {
                showMove(move)
              };
              Thread.sleep(500)

              if (closed) return None
            )
          }
      }
    }

    title = "Bloxorz - Solver - " + game.name
    private val tiles = Tiles.createTilesVector(game)
    private val changed = mutable.ListBuffer[Tiles]()
    contents = new BoxPanel(Orientation.Vertical) {
      background = new Color(232, 232, 232)
      contents += new GridPanel(1, 1) {
        contents += Button("Play") {
          start()
        }
        contents += Button("Save to file") {
          solution match {
            case None =>
              Dialog.showMessage(contents.head, "Game has no solution", title = "Solver")
            case Some(solution) =>
              FileUtil.getPathOfFile(FileDialog.Write) match {
                case Some(path) => FileUtil.saveMovesToFile(path, solution)
                case None =>
              }

          }
        }
      }
      val label = new Label(solution.mkString(", ").replace("List(", "").replace(")", "").trim)
      contents += label

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
    }


    def resetChanged(): Unit = {
      for (f <- changed) {
        f.reset()
      }
      changed.clear()
    }

    def showMove(move: Char): Unit = {
      game.makeAMove(move) match {
        case None =>
        case Some(_) =>
          resetChanged()
          game.currentPosition match {
            case (position, None) =>
              val f = tiles.apply(position._1 * game.N + position._2)
              f.changeColor(Color.YELLOW)
              changed.append(f)
            case (position1, Some(position2)) =>
              val f1 = tiles.apply(position1._1 * game.N + position1._2)
              f1.changeColor(Color.YELLOW)
              changed.append(f1)
              val f2 = tiles.apply(position2._1 * game.N + position2._2)
              f2.changeColor(Color.YELLOW)
              changed.append(f2)
          }

      }
    }

  }
}