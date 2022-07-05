package bloxorz.gui

import bloxorz.{FileDialog, FileUtil}
import bloxorz.gui.App.{listenTo, reactions}
import bloxorz.gui.Command.{FilterOp, tiles}
import bloxorz.Solution

import java.awt
import java.awt.event.ComponentAdapter
import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D
import java.awt.{Color, Graphics2D}
import java.io.*
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.swing.*
import javax.swing.filechooser.{FileFilter, FileNameExtensionFilter}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.{BufferedSource, Source}
import scala.language.postfixOps
import scala.swing.*
import scala.swing.Dialog.{Message, Options, uiString}
import scala.swing.Swing.{EmptyIcon, PeerContainer, nullPeer}
import scala.swing.event.*

abstract class MenuSelection(val label: String) {}

object MenuSelection {
  case object LoadMap extends MenuSelection("Load map")

  case object NewGame extends MenuSelection("Start new game")

  case object MapEditor extends MenuSelection("Edit map")

  case object Solution extends MenuSelection("Get solution")
}

object App extends SimpleSwingApplication {

  def top: MainFrame = new MainFrame {

    title = "Bloxorz"
    contents = new BorderPanel {
      preferredSize = new Dimension(500, 400)
      add(new GridPanel(4, 1) {
        background = Color.GREEN
        var label = new Label(MenuSelection.LoadMap.label)
        label.font = Font("Copperplate Gothic Bold", Font.Style.Bold, 30)
        listenTo(label.mouse.clicks)
        contents += label
        label = new Label(MenuSelection.NewGame.label)
        label.font = Font("Copperplate Gothic Bold", Font.Style.Bold, 30)
        listenTo(label.mouse.clicks)
        contents += label
        label = new Label(MenuSelection.MapEditor.label)
        label.font = Font("Copperplate Gothic Bold", Font.Style.Bold, 30)
        listenTo(label.mouse.clicks)
        contents += label
        label = new Label(MenuSelection.Solution.label)
        label.font = Font("Copperplate Gothic Bold", Font.Style.Bold, 30)
        listenTo(label.mouse.clicks)
        contents += label

        reactions += {
          case MouseClicked(s, _, m, clicks, _) if m == 0 && clicks == 2 && s.isInstanceOf[Label] =>
            val label = s.asInstanceOf[Label]
            println(label.text)

            label.text match {
              case MenuSelection.LoadMap.label =>
                FileUtil.getPathOfFile(FileDialog.Read) match {
                  case Some(path) =>
                    if (path.endsWith(".txt")) {
                      FileUtil.checkMapPattern(path) match {
                        case Some(message) => Dialog.showMessage(null, message, title = "Error loading map")
                        case None => FileUtil.copyMapFromFile(path)
                      }
                    } else {
                      Dialog.showMessage(null, "File extension should be .txt", title = "Error loading map")
                    }
                  case None =>
                }

              case MenuSelection.NewGame.label =>
                SelectMap(MenuSelection.NewGame).top.visible = true
              case MenuSelection.MapEditor.label =>
                SelectMap(MenuSelection.MapEditor).top.visible = true
              case MenuSelection.Solution.label =>
                SelectMap(MenuSelection.Solution).top.visible = true
              case _ => println(s"Invalid label ${label.text}")

            }
        }
      }, BorderPanel.Position.Center)

    }

  }
}

class SelectMap(val menuSelection: MenuSelection) extends SimpleSwingApplication {

  def top: Frame = new Frame {
    title = "Bloxorz - Select Map"

    contents = new BorderPanel {
      private val games = FileUtil.loadMaps()
      preferredSize = new Dimension(500, 400)
      private var rows = games.size / 3
      if (games.size % 3 != 0)
        rows += 1
      add(new GridPanel(rows, 3) {
        preferredSize = new Dimension(500, 400)
        for (game <- games) {
          val title = new Label(game.name)
          title.font = Font("Copperplate Gothic Bold", Font.Style.Plain, 15)
          listenTo(title.mouse.clicks)
          contents += title
        }
        reactions += {
          case MouseClicked(s, _, m, clicks, _) if m == 0 && clicks == 2 && s.isInstanceOf[Label] =>
            val label = s.asInstanceOf[Label]
            games.find(g => g.name == label.text) match {
              case None => // shouldn't happen
              case Some(game) =>
                menuSelection match {
                  case MenuSelection.NewGame =>
                    Play(game).top.visible = true
//                    close()
                  case MenuSelection.MapEditor =>
                    EditMap(game).top.visible = true
                    close()
                  case MenuSelection.Solution =>
                    val solutionWindow = ShowSolution(game, Solution().solve(game))
                    solutionWindow.top.visible = true
//                    close()
                }
            }
        }
      }, BorderPanel.Position.Center)
    }

  }
}
