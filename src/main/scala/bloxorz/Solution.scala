package bloxorz

import scala.annotation.tailrec

class Solution {

  def checkSequence(moveList: List[Char], game: Game): Option[Boolean] = {
    @tailrec
    def checkSequenceRec(moveList: List[Char], visitedPositions: List[Position]): Option[Boolean] = {
      moveList match {
        case Nil => Some(false)
        case h :: t =>
          game.makeAMoveForSolver(h) match {
            case None => None
            case Some(position) if position == game.endPosition => Some(true)
            case Some(position) =>
              if (visitedPositions.contains(position)) None
              else checkSequenceRec(t, position :: visitedPositions)
          }
      }
    }

    game.reset()
    checkSequenceRec(moveList, List())
  }

  val moves = List('d', 'u', 'l', 'r')


  def getMoveChar(move: Int): Char = {
    move match {
      case 0 => 'd'
      case 1 => 'u'
      case 2 => 'l'
      case 3 => 'r'
      case _ => 'x'
    }
  }

  def getMoveInt(move: Char): Int = {
    move match {
      case 'd' => 0
      case 'u' => 1
      case 'l' => 2
      case 'r' => 3
      case _ => 4
    }
  }

  def solve(game: Game): Option[List[Char]] = {

    @tailrec
    def solver(move: Int, moveList: List[Char]): Option[List[Char]] = {
      println("---------------------------------------")
      println("move " + move)
      println("movesList " + moveList.reverse.mkString("", ", ", ""))

      move match {
        case m if m > 3 =>
          moveList match {
            case Nil => None
            case h :: Nil =>
              (getMoveInt(h) + 1) match {
                case 4 => None
                case m => solver(m, Nil)
              }
            case h :: t =>
              (getMoveInt(t.head), getMoveInt(h) + 1) match {
                case (0, 1) => solver(2, t)
                case (2, 3) | (_, 4) => solver(4, t)
                case (3, 2) => solver(3, t)
                case (_, m) => solver(m, t)
              }

          }
        case _ =>
          checkSequence((getMoveChar(move) :: moveList).reverse, game) match {
            case Some(true) =>
              println("Won")
              Some((getMoveChar(move) :: moveList).reverse)
            case None =>
              println("Lost, go back")
              moveList match {
                case Nil => solver(move + 1, moveList)
                case h :: t =>
                  println("None: moveList.head " + getMoveInt(h))
                  (getMoveInt(h), move + 1) match {
                    case (2, 3) | (_, 4) => solver(4, moveList)
                    case (0, 1) => solver(2, moveList)
                    case (3, 2) => solver(3, moveList)
                    case (_, m) => solver(m, moveList)
                  }

              }
            case Some(false) =>
              move match {
                case 1 => solver(1, getMoveChar(move) :: moveList)
                case _ => solver(0, getMoveChar(move) :: moveList)
              }
          }
      }
    }

    solver(0, List())
  }
}
