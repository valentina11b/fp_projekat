import java.awt
import java.awt.event.ComponentAdapter
import java.awt.{Color, Graphics2D}
import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D
import java.io.{BufferedWriter, File, FileOutputStream, FileWriter, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import javax.swing.{Box, JFileChooser, JFrame}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.swing.{Dimension, *}
import scala.swing.event.*

case class PopUpMenuEvent(f: Field, x: Int, y: Int, clicks: Int) extends Event

case class RotateEvent() extends Event

class Field(var color: Color) extends Panel {

  listenTo(mouse.clicks)
  reactions += {
    case MouseClicked(source, p, m, clicks, _) if m == 256 && source.isInstanceOf[Field] => // right click
      publish(PopUpMenuEvent(source.asInstanceOf[Field], p.x, p.y, clicks))
    case MouseClicked(_, p, m, clicks, _) if m == 0 && clicks == 2 =>
      publish(RotateEvent())
    case _ =>
  }

  val originalColor = color

  def reset(): Unit = {
    color = originalColor
    repaint()
  }

  def changeColor(newColor: Color): Unit = {
    color = newColor
    repaint()
  }

  preferredSize = new Dimension(50, 50)

  override def paintComponent(g: Graphics2D) = {
    val d = size
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
      java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(color);
    g.fillRect(0, 0, d.width, d.height);
  }
}


enum MenuSelection {
  case NewGame
  case MapEditor
  case Solution
}

object SecondSwingApp extends SimpleSwingApplication {

  def loadMaps(): List[Game] = {
    BloxorzMap.loadFiles() match {
      case None => List()
      case Some(listOfFiles) =>
        listOfFiles.map(f => {
          BloxorzMap.loadMapFromFile(f)
        }).filter(_.isDefined).map(_.get)
    }
  }

  def top = new MainFrame {

    title = "Bloxorz"
    contents = new BorderPanel {
      preferredSize = new Dimension(500, 400)
      resizable = false
      peer.setSize(new Dimension(500, 400))
      add(new GridPanel(3, 1) {
        background = Color.GREEN
        var label = new Label("Start new game")
        label.font = Font("Copperplate Gothic Bold", Font.Style.Bold, 30)
        listenTo(label.mouse.clicks)
        contents += label
        label = new Label("Edit maps")
        label.font = Font("Copperplate Gothic Bold", Font.Style.Bold, 30)
        listenTo(label.mouse.clicks)
        contents += label
        label = new Label("Get solution")
        label.font = Font("Copperplate Gothic Bold", Font.Style.Bold, 30)
        listenTo(label.mouse.clicks)
        contents += label

        reactions += {
          case MouseClicked(s, p, m, clicks, _) if m == 0 && clicks == 2 && s.isInstanceOf[Label] =>
            val label = s.asInstanceOf[Label]
            println(label.text)
            if (label.text == "Start new game") {
              SelectMap(MenuSelection.NewGame).top.visible = true
            } else if (label.text == "Edit maps") {
              SelectMap(MenuSelection.MapEditor).top.visible = true
            } else if (label.text == "Get solution") {
              SelectMap(MenuSelection.Solution).top.visible = true
            }
        }
      }, BorderPanel.Position.Center)

    }

  }
}

class SelectMap(val menuSelection: MenuSelection) extends SimpleSwingApplication {

  def top = new Frame {
    title = "Bloxorz - Select Map"

    def loadMaps(): List[Game] = {
      BloxorzMap.loadFiles() match {
        case None => List()
        case Some(listOfFiles) =>
          listOfFiles.map(f => {
            BloxorzMap.loadMapFromFile(f)
          }).filter(_.isDefined).map(_.get)
      }
    }

    contents = new BorderPanel {
      val games = loadMaps()
      var nextWindow = 0
      preferredSize = new Dimension(500, 400)
      var rows = games.size / 3
      if (games.size % 3 != 0)
        rows += 1
      add(new GridPanel(rows, 3) {
        preferredSize = new Dimension(500, 400)
        for (game <- games) {
          var title = new Label(game.name)
          title.font = Font("Copperplate Gothic Bold", Font.Style.Plain, 15)
          listenTo(title.mouse.clicks)
          contents += title
        }
        reactions += {
          case MouseClicked(s, p, m, clicks, _) if m == 0 && clicks == 2 && s.isInstanceOf[Label] =>
            val label = s.asInstanceOf[Label]
            games.find(g => (g.name == label.text)) match {
              case None => // shouldnt happen
              case Some(game) => {
                menuSelection match {
                  case MenuSelection.NewGame =>
                    Play(game).top.visible = true
                  case MenuSelection.MapEditor =>
                    EditMap(game).top.visible = true
                  case MenuSelection.Solution =>
                    val solutionWindow = ShowSolution(game, Solution().solve(game))
                    solutionWindow.top.visible = true
                }

              }
            }
        }
      }, BorderPanel.Position.Center)
    }

  }
}

class Play(val game: Game) extends SimpleSwingApplication {

  def top = new Frame {
    title = "Bloxorz"
    var fields = mutable.ListBuffer[Field]()
    var startFiledIndex = 0
    var endFieldIndex = 0
    var changed = mutable.ListBuffer[Field]()
    contents = new BoxPanel(Orientation.Vertical) {
      background = new Color(232, 232, 232)

      contents += new GridPanel(1, 1) {
        contents += Button("Load moves from file") {
          getPathOfFileToRead("Choose file:") match {
            case Some(path) =>
              loadMovesFromFile(path)
            case None =>
          }
        }
      }
      for (line <- game.map) {
        contents += new BoxPanel(Orientation.Horizontal) {
          background = new Color(232, 232, 232)
          for (c <- line) {
            c match {
              case '-' =>
                val field = Field(Color.WHITE)
                fields.append(field)
                contents += field
              case 'S' =>
                startFiledIndex = fields.length
                val field = Field(Color.YELLOW)
                fields.append(field)
                contents += field
              case 'T' =>
                endFieldIndex = fields.length
                val field = Field(Color.GREEN)
                fields.append(field)
                contents += field
              case 'o' =>
                val field = Field(Color.GRAY)
                fields.append(field)
                contents += field
              case '.' =>
                val field = Field(Color.ORANGE)
                fields.append(field)
                contents += field
            }
            peer.add(Box.createHorizontalStrut(2))

          }
        }
        peer.add(Box.createVerticalStrut(2))
      }

      listenTo(keys)
      reactions += {
        case KeyPressed(_, key, _, _)
        =>
          var char = 'x'
          key match {
            case Key.Right => char = 'r'
            case Key.Left => char = 'l'
            case Key.Up => char = 'u'
            case Key.Down => char = 'd'
            case _ => println("Invalid key")
          }
          makeMove(char)
        case _ =>
      }
      focusable = true
      requestFocus()

    }

    def makeMove(move: Char): Unit = {

      if (move == 'x') {
        Dialog.showMessage(contents.head, f"Invalid move ${move} !", title = "Game ended")
        return
      }
      game.makeAMove(move) match {
        case None =>
          println("Sorry, You lost!")
          for (f <- changed) {
            f.reset()
          }
          game.currentPosition match {
            case (position, None) if fields.size > (position._1 * game.map(0).length + position._2) =>
              val f = fields.apply(position._1 * game.map(0).length + position._2)
              f.changeColor(Color.RED)
              changed.append(f)
            case (position1, Some(position2)) =>
              if (fields.size > position1._1 * game.map(0).length + position1._2) {
                val f1 = fields.apply(position1._1 * game.map(0).length + position1._2)
                f1.changeColor(Color.RED)
                changed.append(f1)
              }
              if (fields.size > position2._1 * game.map(0).length + position2._2) {
                val f2 = fields.apply(position2._1 * game.map(0).length + position2._2)
                f2.changeColor(Color.RED)
                changed.append(f2)
              }
            case _ =>
          }
          val res = Dialog.showConfirmation(contents.head, "Sorry, You lost!", optionType = Dialog.Options.OkCancel, title = "Game ended")
          if (res == Dialog.Result.Ok || res == Dialog.Result.Cancel) {
            for (f <- changed) {
              f.reset()
            }
            game.reset()
          }
        case Some(true) =>
          println("Congratulation, You won!")
          for (f <- changed) {
            f.reset()
          }
          game.reset()
          Dialog.showMessage(contents.head, "Congratulation, You won!", title = "Game ended")

        case Some(false) =>
          for (f <- changed) {
            f.reset()
          }
          game.currentPosition match {
            case (position, None) =>
              val f = fields.apply(position._1 * game.map(0).length + position._2)
              f.changeColor(Color.YELLOW)
              changed.append(f)
            case (position1, Some(position2)) =>
              val f1 = fields.apply(position1._1 * game.map(0).length + position1._2)
              f1.changeColor(Color.YELLOW)
              changed.append(f1)
              val f2 = fields.apply(position2._1 * game.map(0).length + position2._2)
              f2.changeColor(Color.YELLOW)
              changed.append(f2)
          }

      }
    }

    def loadMovesFromFile(file: String): Unit = {
      new Thread(new Runnable {
        override def run(): Unit = {
          val source = Source.fromFile(file)
          val lines = source.getLines().toArray
          source.close

          for (line <- lines) {
            if (line.trim.size != 1) {
              Dialog.showMessage(contents.head, s"Invalid line ${line} in file", title = "Invalid file")
              return
            }
            scala.swing.Swing.onEDTWait {
              makeMove(line.charAt(0))
            }
            Thread.sleep(500)
          }

          scala.swing.Swing.onEDTWait {
            contents.head.focusable = true
            contents.head.requestFocus()
          }
        }

      }).start()
    }
  }
}

// Function used for choosing file to open
def chooseFile(): String = {
  val chooser = new JFileChooser(".")
  chooser.showOpenDialog(new JFrame())
  chooser.setDialogTitle("Choose file:")
  val file = chooser.getSelectedFile
  if (file != null) {
    return file.getAbsolutePath
  }
  ""
}

def getPathOfFileToRead(title: String): Option[String] = {
  val chooser = new JFileChooser(".")
  chooser.showOpenDialog(new JFrame())
  chooser.setDialogTitle(title)
  val file = chooser.getSelectedFile
  if (file != null) {
    return Some(file.getAbsolutePath)
  }
  None
}

def getPathOfFileToWrite(title: String): Option[String] = {
  val chooser = new JFileChooser(".")
  chooser.showSaveDialog(new JFrame())
  chooser.setDialogTitle(title)
  val file = chooser.getSelectedFile
  if (file != null) {
    return Some(file.getAbsolutePath)
  }
  None
}

def saveMovesToFile(fileName: String, moves: List[Char]): Unit = {
  val path = fileName + ".txt"
  val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path), StandardCharsets.UTF_8))
  for (move <- moves) {
    bw.write(move + "\n")
  }
  bw.close()
}

class ShowSolution(val game: Game, val solution: Option[List[Char]]) extends SimpleSwingApplication {

  def top = new Frame {

    def start(): Unit = {
      game.reset()
      solution match {
        case None =>
          Dialog.showMessage(contents.head, "Game has no solution", title = "Solver")
        case Some(solution) =>
          new Thread(new Runnable {
            override def run(): Unit = {
              game.reset()
              for (move <- solution) {
                //                if(!top.visible)
                //                  return
                scala.swing.Swing.onEDTWait {
                  showMove(move)
                }
                Thread.sleep(500)
              }
            }
          }).start()
      }
    }

    title = "Bloxorz - Solver"
    var fields = mutable.ListBuffer[Field]()
    var startFiledIndex = 0
    var endFieldIndex = 0
    var changed = mutable.ListBuffer[Field]()
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
              getPathOfFileToWrite("Save file:") match {
                case Some(path) => saveMovesToFile(path, solution)
                case None =>
              }

          }
        }
      }
      var label = new Label(solution.mkString(", ").replace("List(", "").replace(")", "").trim)
      contents += label
      for (line <- game.map) {
        contents += new BoxPanel(Orientation.Horizontal) {
          background = new Color(232, 232, 232)
          for (c <- line) {
            c match {
              case '-' =>
                val field = Field(Color.WHITE)
                fields.append(field)
                contents += field
              case 'S' =>
                startFiledIndex = fields.length
                val field = Field(Color.YELLOW)
                fields.append(field)
                contents += field
              case 'T' =>
                endFieldIndex = fields.length
                val field = Field(Color.GREEN)
                fields.append(field)
                contents += field
              case 'o' =>
                val field = Field(Color.GRAY)
                fields.append(field)
                contents += field
              case '.' =>
                val field = Field(Color.ORANGE)
                fields.append(field)
                contents += field
            }
            peer.add(Box.createHorizontalStrut(2))

          }
        }
        peer.add(Box.createVerticalStrut(2))
      }
    }

    def showMove(move: Char): Unit = {
      println("Got solution event")
      if (move != 'x')
        game.makeAMove(move) match {
          case None =>
            println("Sorry, You lost!")
            for (f <- changed) {
              f.reset()
            }
            game.currentPosition match {
              case (position, None) if fields.size > (position._1 * game.map(0).length + position._2) =>
                val f = fields.apply(position._1 * game.map(0).length + position._2)
                f.changeColor(Color.RED)
                changed.append(f)
              case (position1, Some(position2)) =>
                if (fields.size > position1._1 * game.map(0).length + position1._2) {
                  val f1 = fields.apply(position1._1 * game.map(0).length + position1._2)
                  f1.changeColor(Color.RED)
                  changed.append(f1)
                }
                if (fields.size > position2._1 * game.map(0).length + position2._2) {
                  val f2 = fields.apply(position2._1 * game.map(0).length + position2._2)
                  f2.changeColor(Color.RED)
                  changed.append(f2)
                }
              case _ =>
            }
            val res = Dialog.showConfirmation(contents.head, "Sorry, You lost!", optionType = Dialog.Options.OkCancel, title = "Game ended")
            if (res == Dialog.Result.Ok || res == Dialog.Result.Cancel) {
              for (f <- changed) {
                f.reset()
              }
              game.reset()
            }
          case Some(true) =>
            println("Congratulation, You won!")
            for (f <- changed) {
              f.reset()
            }
            game.reset()
            Dialog.showMessage(contents.head, "Congratulation, You won!", title = "Game ended")

          case Some(false) =>
            for (f <- changed) {
              f.reset()
            }
            game.currentPosition match {
              case (position, None) =>
                val f = fields.apply(position._1 * game.map(0).length + position._2)
                f.changeColor(Color.YELLOW)
                changed.append(f)
              case (position1, Some(position2)) =>
                val f1 = fields.apply(position1._1 * game.map(0).length + position1._2)
                f1.changeColor(Color.YELLOW)
                changed.append(f1)
                val f2 = fields.apply(position2._1 * game.map(0).length + position2._2)
                f2.changeColor(Color.YELLOW)
                changed.append(f2)
            }

        }
    }

  }
}


class EditMap(val game: Game) extends SimpleSwingApplication {

  def top = new Frame {
    title = "Bloxorz - Edit Map"
    var fields = mutable.ListBuffer[Field]()
    var startFiledIndex = 0
    var endFieldIndex = 0
    contents = new BoxPanel(Orientation.Vertical) {
      background = new Color(232, 232, 232)
      for (line <- game.map) {
        contents += new BoxPanel(Orientation.Horizontal) {
          background = new Color(232, 232, 232)
          for (c <- line) {
            c match {
              case '-' =>
                val field = Field(Color.WHITE)
                fields.append(field)
                contents += field
                listenTo(field)
              case 'S' =>
                startFiledIndex = fields.length
                val field = Field(Color.YELLOW)
                fields.append(field)
                contents += field
                listenTo(field)
              case 'T' =>
                endFieldIndex = fields.length
                val field = Field(Color.GREEN)
                fields.append(field)
                contents += field
                listenTo(field)
              case 'o' =>
                val field = Field(Color.GRAY)
                fields.append(field)
                contents += field
                listenTo(field)
              case '.' =>
                val field = Field(Color.ORANGE)
                fields.append(field)
                contents += field
                listenTo(field)
            }
            peer.add(Box.createHorizontalStrut(2))

          }

          reactions += {
            case RotateEvent() =>
              for (f <- fields) {
                if (f.color == Color.ORANGE) {
                  f.changeColor(Color.GRAY)
                }
              }
            case PopUpMenuEvent(field, x, y, clicks) =>
              val popupMenu = new PopupMenu {
                if (field.color == Color.WHITE) {
                  contents += new MenuItem(Action("Add field") {
                    field.changeColor(Color.GRAY)
                  })
                }
                if (field.color == Color.GRAY) {
                  contents += new MenuItem(Action("Make special") {
                    field.changeColor(Color.ORANGE)
                  })
                }
                if (field.color == Color.ORANGE) {
                  contents += new MenuItem(Action("Make ordinary") {
                    field.changeColor(Color.GRAY)
                  })
                }
                if (field.color == Color.YELLOW ||
                  field.color == Color.GREEN
                ) {
                  contents += new MenuItem(Action("Inverzija") {
                    fields(startFiledIndex).changeColor(Color.GREEN)
                    fields(endFieldIndex).changeColor(Color.YELLOW)
                    val tmp = startFiledIndex
                    startFiledIndex = endFieldIndex
                    endFieldIndex = tmp
                  })

                } else {
                  contents += new MenuItem(Action("Set as start") {
                    fields(startFiledIndex).changeColor(Color.GRAY)
                    startFiledIndex = fields.indexOf(field)
                    field.changeColor(Color.YELLOW)
                  })
                  contents += new MenuItem(Action("Set as end") {
                    fields(endFieldIndex).changeColor(Color.GRAY)
                    endFieldIndex = fields.indexOf(field)
                    field.changeColor(Color.GREEN)
                  })

                }

              }.show(field, x, y)

          }
        }
        peer.add(Box.createVerticalStrut(2))
      }

    }
  }
}


object FirstSwingApp extends SimpleSwingApplication {

  def top = new Frame() {
    title = "Bloxorz"
    var fields = mutable.ListBuffer[Field]()
    var startFiledIndex = 0
    var endFieldIndex = 0
    var changed = mutable.ListBuffer[Field]()
    contents = new BoxPanel(Orientation.Vertical) {
      background = new Color(232, 232, 232)
      BloxorzMap.loadMapFromFile("src/main/maps/Example Map.txt") match {
        case None =>
        case Some(game) =>
          for (line <- game.map) {
            contents += new BoxPanel(Orientation.Horizontal) {
              background = new Color(232, 232, 232)
              for (c <- line) {
                c match {
                  case '-' =>
                    val field = Field(Color.WHITE)
                    fields.append(field)
                    contents += field
                    listenTo(field)
                  case 'S' =>
                    startFiledIndex = fields.length
                    val field = Field(Color.YELLOW)
                    fields.append(field)
                    contents += field
                    listenTo(field)
                  case 'T' =>
                    endFieldIndex = fields.length
                    val field = Field(Color.GREEN)
                    fields.append(field)
                    contents += field
                    listenTo(field)
                  case 'o' =>
                    val field = Field(Color.GRAY)
                    fields.append(field)
                    contents += field
                    listenTo(field)
                  case '.' =>
                    val field = Field(Color.ORANGE)
                    fields.append(field)
                    contents += field
                    listenTo(field)
                }
                peer.add(Box.createHorizontalStrut(2))

              }

              reactions += {
                case RotateEvent() =>
                  for (f <- fields) {
                    if (f.color == Color.ORANGE) {
                      f.changeColor(Color.GRAY)
                    }
                  }
                case PopUpMenuEvent(field, x, y, clicks) =>
                  val popupMenu = new PopupMenu {
                    if (field.color == Color.WHITE) {
                      contents += new MenuItem(Action("Add field") {
                        field.changeColor(Color.GRAY)
                      })
                    }
                    if (field.color == Color.GRAY) {
                      contents += new MenuItem(Action("Make special") {
                        field.changeColor(Color.ORANGE)
                      })
                    }
                    if (field.color == Color.ORANGE) {
                      contents += new MenuItem(Action("Make ordinary") {
                        field.changeColor(Color.GRAY)
                      })
                    }
                    if (field.color == Color.YELLOW ||
                      field.color == Color.GREEN
                    ) {
                      contents += new MenuItem(Action("Inverzija") {
                        fields(startFiledIndex).changeColor(Color.GREEN)
                        fields(endFieldIndex).changeColor(Color.YELLOW)
                        val tmp = startFiledIndex
                        startFiledIndex = endFieldIndex
                        endFieldIndex = tmp
                      })

                    } else {
                      contents += new MenuItem(Action("Set as start") {
                        fields(startFiledIndex).changeColor(Color.GRAY)
                        startFiledIndex = fields.indexOf(field)
                        field.changeColor(Color.YELLOW)
                      })
                      contents += new MenuItem(Action("Set as end") {
                        fields(endFieldIndex).changeColor(Color.GRAY)
                        endFieldIndex = fields.indexOf(field)
                        field.changeColor(Color.GREEN)
                      })

                    }

                  }.show(field, x, y)

              }
            }
            peer.add(Box.createVerticalStrut(2))
          }

          listenTo(keys)
          reactions += {
            case KeyPressed(_, key, _, _)
            =>
              var char = 'x'
              key match {
                case Key.Right => char = 'r'
                case Key.Left => char = 'l'
                case Key.Up => char = 'u'
                case Key.Down => char = 'd'
                case _ => println("Invalid key")
              }
              if (char != 'x')
                game.makeAMove(char) match {
                  case None =>
                    println("Sorry, You lost!")
                    for (f <- changed) {
                      f.reset()
                    }
                    game.currentPosition match {
                      case (position, None) =>
                        val f = fields.apply(position._1 * game.map(0).length + position._2)
                        f.changeColor(Color.RED)
                        changed.append(f)
                      case (position1, Some(position2)) =>
                        val f1 = fields.apply(position1._1 * game.map(0).length + position1._2)
                        f1.changeColor(Color.RED)
                        changed.append(f1)
                        val f2 = fields.apply(position2._1 * game.map(0).length + position2._2)
                        f2.changeColor(Color.RED)
                        changed.append(f2)
                    }
                    val res = Dialog.showConfirmation(contents.head, "Sorry, You lost!", optionType = Dialog.Options.OkCancel, title = "Game ended")
                    if (res == Dialog.Result.Ok || res == Dialog.Result.Cancel) {
                      for (f <- changed) {
                        f.reset()
                      }
                      game.reset()
                    }
                  case Some(true) =>
                    println("Congratulation, You won!")
                    for (f <- changed) {
                      f.reset()
                    }
                    game.reset()
                    Dialog.showMessage(contents.head, "Congratulation, You won!", title = "Game ended")

                  case Some(false) =>
                    for (f <- changed) {
                      f.reset()
                    }
                    game.currentPosition match {
                      case (position, None) =>
                        val f = fields.apply(position._1 * game.map(0).length + position._2)
                        f.changeColor(Color.YELLOW)
                        changed.append(f)
                      case (position1, Some(position2)) =>
                        val f1 = fields.apply(position1._1 * game.map(0).length + position1._2)
                        f1.changeColor(Color.YELLOW)
                        changed.append(f1)
                        val f2 = fields.apply(position2._1 * game.map(0).length + position2._2)
                        f2.changeColor(Color.YELLOW)
                        changed.append(f2)
                    }

                }
            case _ =>
          }
          focusable = true
          requestFocus()
      }


    }
  }
}

