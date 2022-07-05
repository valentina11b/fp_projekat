package bloxorz.gui

import bloxorz.{FileUtil, Game}
import bloxorz.gui.Tiles

import java.awt.Color
import javax.swing.Box
import scala.annotation.tailrec
import scala.collection.mutable
import scala.swing.*
import scala.swing.Dialog.Message

abstract class Command(val name: String) {
  override def toString: String = name
}

class NCommand(name: String, val t: (Tiles, Int) => Unit) extends Command(name)

class TileCommand(name: String, val t: Tiles => Unit) extends Command(name)

class MapCommand(name: String, val t: () => Unit) extends Command(name)

object Command {

  var tiles: Vector[Tiles] = Vector()
  var M: Int = 0
  var N: Int = 0
  var startTilesIndex: Int = 0
  var endTilesIndex: Int = 0

  def setUp(game: Game): Unit = {
    this.tiles = Tiles.createTilesVector(game)
    this.M = game.M
    this.N = game.N
    this.startTilesIndex = game.startPosition._1._1 * game.N + game.startPosition._1._2
    this.endTilesIndex = game.endPosition._1._1 * game.N + game.endPosition._1._2
  }

  case object AddToEdge extends TileCommand("Add tile on edge", (tile: Tiles) =>
    if (tile.color == Color.WHITE && checkIfEdge(tile))
      tile.changeColor(Color.GRAY))

  case object RemoveFromEdge extends TileCommand("Remove from the edge", (tile: Tiles) =>
    if ((tile.color == Color.GRAY || tile.color == Color.ORANGE) && checkIfOnEdge(tile))
      tile.changeColor(Color.WHITE))

  case object OrdinaryToSpecial extends TileCommand("Ordinary To Special", (tile: Tiles) =>
    if (tile.color == Color.GRAY)
      tile.changeColor(Color.ORANGE))

  case object SpecialToOrdinary extends TileCommand("Special To Ordinary", (tile: Tiles) =>
    if (tile.color == Color.ORANGE)
      tile.changeColor(Color.GRAY))

  case object ChangeStart extends TileCommand("Set as Start", (tile: Tiles) =>
    if (tile.color != Color.YELLOW &&
      tile.color != Color.GREEN
    ) {
      tiles(startTilesIndex).changeColor(Color.GRAY)
      startTilesIndex = tiles.indexOf(tile)
      tile.changeColor(Color.YELLOW)
    })

  case object ChangeEnd extends TileCommand("Set as End", (tile: Tiles) =>
    if (tile.color != Color.YELLOW &&
      tile.color != Color.GREEN
    ) {
      tiles(endTilesIndex).changeColor(Color.GRAY)
      endTilesIndex = tiles.indexOf(tile)
      tile.changeColor(Color.GREEN)
    }
  )

  case object Invert extends MapCommand("Invert", () => {
    tiles(startTilesIndex).changeColor(Color.GREEN)
    tiles(endTilesIndex).changeColor(Color.YELLOW)
    val tmp = startTilesIndex
    startTilesIndex = endTilesIndex
    endTilesIndex = tmp
  }
  )

  case object Exchange extends MapCommand("Make special tiles ordinary",
    () => {
      for (f <- tiles) {
        if (f.color == Color.ORANGE) {
          f.changeColor(Color.GRAY)
        }
      }
    })


  case object FilterOp extends NCommand("Filter", (tile: Tiles, n: Int) => {
    for (n <- 1 to n) {
      if ((tile.i + n < M && tiles((tile.i + n) * N + tile.j).color == Color.ORANGE)
        || (tile.j + n < N && tiles(tile.i * N + tile.j + n).color == Color.ORANGE)
        || (tile.i - n > 0 && tiles((tile.i - n) * N + tile.j).color == Color.ORANGE)
        || (tile.j - n > 0 && tiles(tile.i * N + tile.j - n).color == Color.ORANGE)
      ) {
        tile.changeColor(Color.GRAY)
      }
    }
  }
  )

  case object FilterSeq extends TileCommand("Filter", (tile: Tiles) => {
    val selection = Dialog.showInput(null, null, "Enter N for Filter", Dialog.Message.Plain, null, Nil, "1")
    val (i, j) = (tile.i, tile.j)
    selection match {
      case None => Dialog.showMessage(null, "Invalid N for filtering", title = "Error")
      case Some(n) =>
        for (n <- 1 to n.toInt) {
          if ((i + n < M && tiles((i + n) * N + j).color == Color.ORANGE)
            || (j + n < N && tiles(i * N + j + n).color == Color.ORANGE)
            || (i - n > 0 && tiles((i - n) * N + j).color == Color.ORANGE)
            || (j - n > 0 && tiles(i * N + j - n).color == Color.ORANGE)
          ) {
            tile.changeColor(Color.GRAY)
          }
        }
    }
  }
  )

  def checkIfEdge(tile: Tiles): Boolean = {
    val i = tile.i
    val j = tile.j
    if ((i + 1 < M && tiles((i + 1) * N + j).color != Color.WHITE)
      || (i - 1 >= 0 && tiles((i - 1) * N + j).color != Color.WHITE)
      || (j + 1 < N && tiles(i * N + j + 1).color != Color.WHITE)
      || (j - 1 >= 0 && tiles(i * N + j - 1).color != Color.WHITE)
    ) {
      true
    } else {
      false
    }
  }

  def checkIfOnEdge(tile: Tiles): Boolean = {
    val i = tile.i
    val j = tile.j
    if (i + 1 == M || j + 1 == N || i == 0 || j == 0) {
      true
    } else if (tiles((i + 1) * N + j).color == Color.WHITE
      || tiles((i - 1) * N + j).color == Color.WHITE
      || tiles(i * N + j + 1).color == Color.WHITE
      || tiles(i * N + j - 1).color == Color.WHITE
    ) {
      if (tiles((i - 1) * N + j).color != Color.WHITE && tiles((i + 1) * N + j).color != Color.WHITE
        && tiles(i * N + j + 1).color == Color.WHITE && tiles(i * N + j - 1).color == Color.WHITE

        ||

        tiles((i - 1) * N + j).color == Color.WHITE && tiles((i + 1) * N + j).color == Color.WHITE
          && tiles(i * N + j + 1).color != Color.WHITE && tiles(i * N + j - 1).color != Color.WHITE

      ) {
        false
      } else {
        true
      }
    } else {
      false
    }
  }

  var operations: mutable.ListBuffer[Command] = mutable.ListBuffer(
    AddToEdge,
    RemoveFromEdge,
    OrdinaryToSpecial,
    SpecialToOrdinary,
    ChangeStart,
    ChangeEnd,
    Invert,
    Exchange,
    FilterOp
  )

  var seqOp: mutable.ListBuffer[Command] = mutable.ListBuffer(
    AddToEdge,
    RemoveFromEdge,
    OrdinaryToSpecial,
    SpecialToOrdinary,
    ChangeStart,
    ChangeEnd,
    Invert,
    Exchange,
    FilterSeq
  )
}

class EditMap(val game: Game) extends SimpleSwingApplication {

  def top: Frame = new Frame {
    title = "Bloxorz - Edit Map"
    Command.setUp(game)

    class CustomOperation(name: String, val commands: Seq[Command]) extends Command(name) {
      @tailrec
      private def applyF(operands: List[Option[Tiles]], commands: List[Command], nOperands: List[Int]): Unit = {

        (commands, operands) match {
          case (h :: t, hO :: tO) =>
            h match {
              case c: NCommand =>
                hO match {
                  case None =>
                  case Some(value) =>
                    nOperands match {
                      case Nil =>
                      case hN :: tN =>
                        scala.swing.Swing.onEDT {
                          c.t(value, hN)
                        }
                        applyF(tO, t, tN)
                    }
                }
              case c: TileCommand =>
                hO match {
                  case None =>
                  case Some(value) =>
                    scala.swing.Swing.onEDT {
                      c.t(value)
                    }
                    applyF(tO, t, nOperands)
                }
              case c: MapCommand =>
                scala.swing.Swing.onEDT {
                  c.t()
                }
                applyF(tO, t, nOperands)
              case _ =>
            }
          case _ =>
        }

      }

      def f(): Unit = {
        val (customs, nonCustom) = commands partition (_.isInstanceOf[CustomOperation])
        val allCommands = nonCustom.toList ::: customs.flatMap(c => c.asInstanceOf[CustomOperation].commands).toList

        val (nCommands, nonNCommands) = allCommands partition (c => c.isInstanceOf[NCommand])

        val operandsForNCmds = for {comm <- nCommands
                                    n: Int = {
                                      val selection = Dialog.showInput(null, null, s"Enter N for ${comm.name}", Dialog.Message.Plain, null, Nil, "1")
                                      selection match {
                                        case None => 0
                                        case Some(value) => value.toInt
                                      }
                                    }
                                    list = Command.tiles.filter(tile => tile.color == Color.ORANGE);
                                    tile: Option[Tiles] =
                                      if (list.nonEmpty)
                                        Dialog.showInput(null, null, s"Select Tile for ${comm.name}", Dialog.Message.Plain, null, list, list(0))
                                      else None

                                    } yield (n, tile)
        val operands = for {comm <- nonNCommands
                            (list, name) = comm match {
                              case c: TileCommand =>
                                c match {
                                  case Command.AddToEdge => (Command.tiles.filter(tile => tile.color == Color.WHITE && Command.checkIfEdge(tile)), c.name)
                                  case Command.RemoveFromEdge => (Command.tiles.filter(tile => (tile.color == Color.GRAY || tile.color == Color.ORANGE) && Command.checkIfOnEdge(tile)), c.name)
                                  case Command.OrdinaryToSpecial => (Command.tiles.filter(tile => tile.color == Color.GRAY), c.name)
                                  case Command.SpecialToOrdinary => (Command.tiles.filter(tile => tile.color == Color.ORANGE), c.name)
                                  case Command.ChangeStart | Command.ChangeEnd => (Command.tiles.filter(tile => tile.color != Color.GREEN && tile.color != Color.YELLOW), c.name)
                                  case _ => (Nil, c.name)
                                }
                              case _ => (Nil, "")
                            }

                            selection =
                              if (list.nonEmpty)
                                Dialog.showInput(null, null, s"Select Tile for $name", Dialog.Message.Plain, null, list, list(0))
                              else None

                            } yield selection


        applyF(operandsForNCmds.map(o => o._2) ::: operands, nCommands ::: nonNCommands, operandsForNCmds.map(o => o._1))
      }
    }

    class CustomSeqOperation(name: String, val commands: Seq[Command]) extends Command(name) {
      def f(): Unit = {
        for (comm <- commands) {
          comm match {
            case c: TileCommand =>
              val list = c match {
                case Command.AddToEdge => Command.tiles.filter(tile => tile.color == Color.WHITE && Command.checkIfEdge(tile))
                case Command.RemoveFromEdge => Command.tiles.filter(tile => (tile.color == Color.GRAY || tile.color == Color.ORANGE) && Command.checkIfOnEdge(tile))
                case Command.OrdinaryToSpecial => Command.tiles.filter(tile => tile.color == Color.GRAY)
                case Command.SpecialToOrdinary => Command.tiles.filter(tile => tile.color == Color.ORANGE)
                case Command.ChangeStart | Command.ChangeEnd => Command.tiles.filter(tile => tile.color != Color.GREEN && tile.color != Color.YELLOW)
                case Command.FilterSeq =>
                  Command.tiles.filter(tile => tile.color == Color.ORANGE)
              }
              if (list.nonEmpty) {
                val selection = Dialog.showInput(null, null, s"Select Tile for ${c.name}", Dialog.Message.Plain, null, list, list(0))
                selection match {
                  case None =>
                  case Some(tile) =>
                    c.t(tile)
                }
              } else {
                Dialog.showMessage(null, s"No tiles for operation ${c.name}", "Warning", Dialog.Message.Warning)
              }

            case c: MapCommand =>
              scala.swing.Swing.onEDT {
                c.t()
              }
            case c: CustomSeqOperation =>
              c.f()
            case _ =>
          }
        }
      }
    }


    class CreateCustomOp(val op: Boolean) extends SimpleSwingApplication {

      def top: Frame = new Frame {
        title = if (op) "Bloxorz - Create Operation" else "Bloxorz - Create Operation Seq"
        contents = new BoxPanel(Orientation.Horizontal) {
          peer.add(Box.createHorizontalStrut(30))
          contents += new BoxPanel(Orientation.Vertical) {
            peer.add(Box.createVerticalStrut(10))
            private val nameField = new TextField()
            nameField.text = ""
            private val list = if (op) swing.ListView[Command](Command.operations) else swing.ListView[Command](Command.seqOp)
            contents += new BoxPanel(Orientation.Horizontal) {
              contents += new Label("Name")
              contents += Swing.HStrut(5)
              contents += nameField
            }
            peer.add(Box.createVerticalStrut(10))
            contents += list
            peer.add(Box.createVerticalStrut(10))
            contents += new GridPanel(1, 1) {
              contents += Button("Save") {
                if (list.selection.items.size > 0 && !nameField.text.isBlank) {
                  if (op)
                    Command.operations.append(CustomOperation(nameField.text, list.selection.items))
                  else
                    Command.seqOp.append(CustomSeqOperation(nameField.text, list.selection.items))
                  close()
                } else {
                  Dialog.showMessage(contents.head, "Please enter name and select at least one option", title = "Failed", messageType = Message.Error)
                }

              }
            }
            peer.add(Box.createVerticalStrut(10))
          }

          peer.add(Box.createHorizontalStrut(30))
        }

      }
    }

    contents = new BoxPanel(Orientation.Vertical) {
      background = new Color(232, 232, 232)
      contents += new GridPanel(1, 1) {
        contents += Button("Save and Exit") {
          FileUtil.saveMapToFile(game.name, Command.tiles)
          close()
        }
      }

      for ((line, i) <- game.map.zipWithIndex) {
        contents += new BoxPanel(Orientation.Horizontal) {
          background = new Color(232, 232, 232)

          for ((_, j) <- line.zipWithIndex) {
            val tile = Command.tiles(i * line.length + j)
            contents += tile
            listenTo(tile)
            peer.add(Box.createHorizontalStrut(2))
          }

          reactions += {
//            case RotateEvent() =>
//              for (f <- Command.tiles) {
//                if (f.color == Color.ORANGE) {
//                  f.changeColor(Color.GRAY)
//                }
//              }
            case PopUpMenuEvent(tile, x, y, _) =>
              new PopupMenu {
                if (tile.color == Color.WHITE && Command.checkIfEdge(tile)) {
                  contents += new MenuItem(Action("Add tile") {
                    Command.AddToEdge.t(tile)
                  })
                }

                if (tile.color == Color.GRAY || tile.color == Color.ORANGE) {
                  if (Command.checkIfOnEdge(tile)) {
                    contents += new MenuItem(Action("Remove") {
                      Command.RemoveFromEdge.t(tile)
                    })
                  }
                }

                if (tile.color == Color.GRAY) {
                  contents += new MenuItem(Action("Make special") {
                    Command.OrdinaryToSpecial.t(tile)
                  })
                }
                if (tile.color == Color.ORANGE) {
                  contents += new MenuItem(Action("Make ordinary") {
                    Command.SpecialToOrdinary.t(tile)
                  })

                  contents += new MenuItem(Action("Filter") {
                    Command.FilterSeq.t(tile)
                  })
                }
                if (tile.color != Color.YELLOW &&
                  tile.color != Color.GREEN
                ) {
                  contents += new MenuItem(Action("Set as start") {
                    Command.ChangeStart.t(tile)
                  })
                  contents += new MenuItem(Action("Set as end") {
                    Command.ChangeEnd.t(tile)
                  })

                }

              }.show(tile, x, y)

          }
        }
        peer.add(Box.createVerticalStrut(2))
      }

      contents += new GridPanel(3, 2) {
        val buttonApplyOp: Button = Button("Apply operation") {
          val list = Command.operations.filter(command => command.isInstanceOf[CustomOperation])
          if (list.nonEmpty)
            Dialog.showInput(null, null, "Select operation", Dialog.Message.Plain, null, list, list(0)) match {
              case None =>
              case Some(op) => op.asInstanceOf[CustomOperation].f()
            }
        }
        contents += buttonApplyOp
        val buttonApplySeq: Button = Button("Apply sequence") {
          val list = Command.seqOp.filter(command => command.isInstanceOf[CustomSeqOperation])
          if (list.nonEmpty)
            Dialog.showInput(null, null, "Select operation sequence", Dialog.Message.Plain, null, list, list(0)) match {
              case None =>
              case Some(op) => op.asInstanceOf[CustomSeqOperation].f()
            }
        }

        contents += buttonApplySeq
        contents += Button("Create operation") {
          CreateCustomOp(true).top.visible = true
        }
        contents += Button("Create sequence") {
          CreateCustomOp(false).top.visible = true
        }
        contents += Button("Invert start and end") {
          Command.Invert.t()
        }
        contents += Button("Make special ones ordinary") {
          Command.Exchange.t()
        }
      }
    }
  }
}