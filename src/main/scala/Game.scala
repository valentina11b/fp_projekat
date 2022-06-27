class Game(
            val name:String,
            val startPosition: ((Int, Int), Option[(Int, Int)]),
            val endPosition: ((Int, Int), Option[(Int, Int)]),
            var allowedPositions: List[(Int, Int)],
            var specialPositions: List[(Int, Int)],
            val map: Array[String]
          ) {

  var currentPosition: ((Int, Int), Option[(Int, Int)]) = startPosition

  def reset() : Unit = {
    currentPosition = startPosition
  }

  def mapString(): String = {
    val currentMap = map.clone()
    currentMap.update(currentPosition._1._1, currentMap.apply(currentPosition._1._1).updated(currentPosition._1._2, 'X'))
    currentPosition._2 match {
      case Some(position) =>
        currentMap.update(position._1, currentMap.apply(position._1).updated(position._2, 'X'))
      case None =>
    }
    currentMap.mkString("", "\n", "")
  }

  def printMap(): Unit = {
    println(mapString())
  }

  def validPosition(position: ((Int, Int), Option[(Int, Int)])): Boolean = {
    position match {
      case (position, None) =>
        allowedPositions.contains(position)
      case (position1, Some(position2)) =>
        ((allowedPositions.contains(position1) && allowedPositions.contains(position2))
          ||
          (specialPositions.contains(position1) && allowedPositions.contains(position2))
          ||
//          (specialPositions.contains(position1) && specialPositions.contains(position2)) // TODO: what happens if block is on two special positions
//            ||
          (allowedPositions.contains(position1) && specialPositions.contains(position2)))
    }
  }


  def makeAMoveForSolver(move: Char): Option[((Int, Int), Option[(Int, Int)])] = {
    require(move == 'd' || move == 'g' || move == 'l' || move == 'r')

    var nextPosition: ((Int, Int), Option[(Int, Int)]) = ((0, 0), None)

    (currentPosition, move) match {
      case ((position, None), 'd') =>
        nextPosition = ((position._1 + 2, position._2), Some((position._1 + 1, position._2)))
      case ((position, None), 'g') =>
        nextPosition = ((position._1 - 1, position._2), Some(position._1 - 2, position._2))
      case ((position, None), 'l') =>
        nextPosition = ((position._1, position._2 - 2), Some((position._1, position._2 - 1)))
      case ((position, None), 'r') =>
        nextPosition = ((position._1, position._2 + 1), Some(position._1, position._2 + 2))

      case ((position1, Some(position2)), 'd') =>
        if (position1._1 == position2._1)
          nextPosition = ((position1._1 + 1, position1._2), Some(position2._1 + 1, position2._2))
        else
          nextPosition = ((position1._1 + 1, position1._2), None)
      case ((position1, Some(position2)), 'g') =>
        if (position1._1 == position2._1)
          nextPosition = ((position1._1 - 1, position1._2), Some(position2._1 - 1, position2._2))
        else
          nextPosition = ((position2._1 - 1, position2._2), None)
      case ((position1, Some(position2)), 'l') =>
        if (position1._2 == position2._2)
          nextPosition = ((position1._1, position1._2 - 1), Some(position2._1, position2._2 - 1))
        else
          nextPosition = ((position1._1, position1._2 - 1), None)
      case ((position1, Some(position2)), 'r') =>
        if (position1._2 == position2._2)
          nextPosition = ((position1._1, position1._2 + 1), Some(position2._1, position2._2 + 1))
        else
          nextPosition = ((position2._1, position2._2 + 1), None)

      case _ => None
    }
    //    println(f"Current position $currentPosition, next move : $nextPosition")
    currentPosition = nextPosition
    if (validPosition(nextPosition)) {
      Some(currentPosition)
    } else {
      None
    }
  }

  def makeAMove(move: Char): Option[Boolean] = {
    makeAMoveForSolver(move) match {
      case None => None
      case Some(position) if position == endPosition => Some(true)
      case Some(position) if position != endPosition => Some(false)
    }
  }

}
