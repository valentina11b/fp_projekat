package bloxorz

type Position = ((Int, Int), Option[(Int, Int)])

class Game(
            val name: String,
            val startPosition: Position,
            val endPosition: Position,
            var allowedPositions: List[(Int, Int)],
            var specialPositions: List[(Int, Int)],
            val map: Vector[Vector[Char]]
          ) {

  var currentPosition: Position = startPosition
  val M: Int = map.length
  val N: Int = map(0).length

  def reset(): Unit = {
    currentPosition = startPosition
  }

  def coordinateInMap(coordinate: (Int, Int)): Boolean = {
    coordinate._1 >= 0 && coordinate._1 < M && coordinate._2 >= 0 && coordinate._2 < N
  }

  def validPosition(position: Position): Boolean = {
    position match {
      case (position, None) =>
        allowedPositions.contains(position)
      case (position1, Some(position2)) =>
        (allowedPositions.contains(position1) && allowedPositions.contains(position2))
          ||
          (specialPositions.contains(position1) && allowedPositions.contains(position2))
          ||
          (specialPositions.contains(position1) && specialPositions.contains(position2)) // TODO: what happens if block is on two special positions
          ||
          (allowedPositions.contains(position1) && specialPositions.contains(position2))
    }
  }


  def makeAMoveForSolver(move: Char): Option[Position] = {
    require(move == 'd' || move == 'u' || move == 'l' || move == 'r')

    currentPosition = (currentPosition, move) match {
      case ((position, None), 'd') =>
        ((position._1 + 2, position._2), Some((position._1 + 1, position._2)))
      case ((position, None), 'u') =>
        ((position._1 - 1, position._2), Some(position._1 - 2, position._2))
      case ((position, None), 'l') =>
        ((position._1, position._2 - 2), Some((position._1, position._2 - 1)))
      case ((position, None), 'r') =>
        ((position._1, position._2 + 1), Some(position._1, position._2 + 2))

      case ((position1, Some(position2)), 'd') =>
        if (position1._1 == position2._1)
          ((position1._1 + 1, position1._2), Some(position2._1 + 1, position2._2))
        else
          ((position1._1 + 1, position1._2), None)
      case ((position1, Some(position2)), 'u') =>
        if (position1._1 == position2._1)
          ((position1._1 - 1, position1._2), Some(position2._1 - 1, position2._2))
        else
          ((position2._1 - 1, position2._2), None)
      case ((position1, Some(position2)), 'l') =>
        if (position1._2 == position2._2)
          ((position1._1, position1._2 - 1), Some(position2._1, position2._2 - 1))
        else
          ((position1._1, position1._2 - 1), None)
      case ((position1, Some(position2)), 'r') =>
        if (position1._2 == position2._2)
          ((position1._1, position1._2 + 1), Some(position2._1, position2._2 + 1))
        else
          ((position2._1, position2._2 + 1), None)

      case _ => return None
    }

    Some(currentPosition).filter(validPosition)
  }

  def makeAMove(move: Char): Option[Boolean] = {
    makeAMoveForSolver(move) match {
      case None => None // invalid position
      case Some(position) => Some(position == endPosition)
    }
  }

}
