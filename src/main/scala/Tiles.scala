import java.awt.{Color, Graphics2D}
import scala.swing.*
import scala.swing.event.{Event, MouseClicked}

case class PopUpMenuEvent(t: Tiles, x: Int, y: Int, clicks: Int) extends Event

case class RotateEvent() extends Event

class Tiles(var color: Color, val i: Int, val j: Int) extends Panel {

  listenTo(mouse.clicks)
  reactions += {
    case MouseClicked(source, p, m, clicks, _) if m == 256 && source.isInstanceOf[Tiles] => // right click
      publish(PopUpMenuEvent(source.asInstanceOf[Tiles], p.x, p.y, clicks))
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

  override def toString: String = s"($i, $j)"
}

object Tiles {
  def createTilesVector(game: Game): Vector[Tiles] = {
    for {(line, i) <- game.map.zipWithIndex; (c,j) <- line.zipWithIndex
         tile = c match {
           case '-' =>
             Tiles(Color.WHITE, i, j)
           case 'S' =>
             Tiles(Color.YELLOW, i, j)
           case 'T' =>
             Tiles(Color.GREEN, i, j)
           case 'o' =>
             Tiles(Color.GRAY, i, j)
           case '.' =>
             Tiles(Color.ORANGE, i, j)
         }
         } yield tile;
  }
}