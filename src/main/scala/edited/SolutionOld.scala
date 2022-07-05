//package edited
//
//import java.util.Date
//import scala.::
//import scala.annotation.tailrec
//import scala.collection.mutable
//
//class SolutionOld {
//
//  println("Start program")
//
//  def socialConnection(network: Map[String, Set[String]],
//                       a: String, b: String): Boolean = {
//    @tailrec
//    def bfs(target: String,
//            consideredPeople: Set[String],
//            discoveredPeople: Set[String]): Boolean = {
//      if (discoveredPeople.isEmpty) false
//      else {
//        val person = discoveredPeople.head
//        if (person == target) true
//        else if (consideredPeople.contains(person))
//          bfs(target, consideredPeople, discoveredPeople.tail)
//        else bfs(target, consideredPeople + person,
//          discoveredPeople.tail ++ network(person))
//      }
//    }
//
//    bfs(b, Set(), network(a) + a)
//  }
//
//
//  def selectAMap(): Option[BloxorzMap] = {
//    val list = BloxorzMap.loadFiles()
//    list match {
//      case Some(list) =>
//        val maps: Array[BloxorzMap] = list.map(
//          el => BloxorzMap.loadMapAsCharMatrix(el)
//        ).toArray
//        for ((map, i) <- maps.zipWithIndex) {
//          val index = i + 1
//          println(f"$index) ${map.name}")
//          map.print()
//        }
//
//        val line = Console.in.readLine()
//        if (line.nonEmpty) {
//          val mapNum = line.charAt(0) - '0'
//          mapNum match {
//            case index if (index <= maps.length && index > 0) =>
//              return Some(maps(index - 1))
//            case index => println(f"Invalid map number $index")
//          }
//
//        }
//
//      case None => println("No maps")
//    }
//    None
//  }
//
//  def getCoordinate(M: Int, N: Int): Option[(Int, Int)] = {
//    println("Write coordinate: i,j")
//    val line = Console.in.readLine().trim
//    if (line.nonEmpty) {
//      val arr = line.split(',')
//      if (arr.length == 2) {
//        try {
//          val i = arr(0).toInt
//          val j = arr(1).toInt
//          if (i > 0 && j > 0 && i < N && j < M) {
//            return Some((i, j))
//          }
//        } catch {
//          case e: Exception => return None
//        }
//      }
//    }
//    None
//  }
//
//  def isConnected(pickedPlace: (Int, Int), map: BloxorzMap): Boolean = {
//    pickedPlace match {
//      case (i, j) =>
//        if (map.map(i - 1)(j) == '-' &&
//          map.map(i + 1)(j) == '-' &&
//          map.map(i)(j - 1) == '-' &&
//          map.map(i)(j + 1) == '-') false
//        else true
//    }
//  }
//
//  def checkIfOnEdge(pickedPlace: (Int, Int), map: BloxorzMap): Boolean = {
//    val N = map.map.length - 1
//    val M = map.map(0).length - 1
//    pickedPlace match {
//      case (i, j) if i == N | j == M | i == 0 | j == 0 => true
//      case (i, j) =>
//        if (map.map(i - 1)(j) == '-' |
//          map.map(i + 1)(j) == '-' |
//          map.map(i)(j - 1) == '-' |
//          map.map(i)(j + 1) == '-') true
//        else false
//    }
//  }
//
//  def changeMap(map: BloxorzMap): Option[BloxorzMap] = {
//    println("Select command")
//    println("1. Uklanjanje zadate ploče sa ivice terena")
//    println("2. Dodavanje ploče na zadatu poziciju na ivici terena")
//    println("3. Zamena obične ploče na zadatoj poziciji specijalnom")
//    println("4. Zamena specijalne ploče na zadatoj poziciji običnom")
//    println("5. Postavljanje startne pozicije na zadato polje, pri čemu se originalna startna pozicija menja običnom pločom.")
//    println("6. Postavljanje ciljne pozicije na zadato polje, pri čemu se originalna ciljna pozicija menja običnom pločom.")
//    println("8. Inverzija: rezultujuća mapa dobija se tako što se na originalnoj mapi zamene startna i ciljna pozicija.")
//    println("9. Zamena: rezultujuća mapa dobija se tako što se na originalnoj mapi sve specijalne ploče pretvaraju u obične ploče.")
//    val line = Console.in.readLine()
//    if (line.nonEmpty) {
//      val command = line.charAt(0) - '0'
//      command match {
//        case 1 | 2 | 3 | 4 | 5 | 6 =>
//          getCoordinate(map.map.length, map.map(0).length) match {
//            case None =>
//              println("Wrong coordinate")
//            case Some(pickedPlace) =>
//              command match {
//                case 1 =>
//                  if (checkIfOnEdge(pickedPlace, map)) {
//                    if (map.map(pickedPlace._1)(pickedPlace._2) == 'o') {
//                      val newMap = BloxorzMap(f"${map.name}_${Date().getTime}", map.map.clone())
//                      newMap.replace(pickedPlace, '-')
//                      return Some(newMap)
//                    }
//                  }
//                case 2 =>
//                  if (checkIfOnEdge(pickedPlace, map)) {
//                    if (map.map(pickedPlace._1)(pickedPlace._2) == '-') {
//                      val newMap = BloxorzMap(f"${map.name}_${Date().getTime}", map.map.clone())
//                      newMap.replace(pickedPlace, 'o')
//                      return Some(newMap)
//                    }
//                  }
//                case 3 =>
//                  if (map.map(pickedPlace._1)(pickedPlace._2) == 'o') {
//                    val newMap = BloxorzMap(f"${map.name}_${Date().getTime}", map.map.clone())
//                    newMap.replace(pickedPlace, '.')
//                    return Some(newMap)
//                  }
//                case 4 =>
//                  if (map.map(pickedPlace._1)(pickedPlace._2) == '.') {
//                    val newMap = BloxorzMap(f"${map.name}_${Date().getTime}", map.map.clone())
//                    newMap.replace(pickedPlace, 'o')
//                    return Some(newMap)
//                  }
//                case 5 | 6 =>
//                  if (isConnected(pickedPlace, map)) {
//                    val newMap = BloxorzMap(f"${map.name}_${Date().getTime}", map.map.clone())
//                    newMap.replaceStartEnd(command == 5, pickedPlace)
//                    return Some(newMap)
//                  }
//
//                case _ =>
//              }
//          }
//        case 8 =>
//          val newMap = BloxorzMap(f"${map.name}_${Date().getTime}", map.map.clone())
//          newMap.invertStartEnd()
//          return Some(newMap)
//
//        case 9 =>
//          val newMap = BloxorzMap(f"${map.name}_${Date().getTime}", map.map.clone())
//          newMap.reverse()
//          return Some(newMap)
//
//
//        case _ =>
//      }
//    }
//    None
//  }
//
//  //  println("Select command")
//  //  println("1. Load map from file")
//  //  println("2. Start new game")
//  //  println("3. Create new map from existing ones")
//  //  val line = Console.in.readLine().trim
//  //  println(line)
//  //  if (line.nonEmpty) {
//  //    val command = line.charAt(0)
//  //    command match {
//  //      case '1' =>
//  //      case '2' =>
//  //      case '3' =>
//  //        println("Select a map")
//  //        selectAMap() match {
//  //          case Some(map) =>
//  //            changeMap(map) match {
//  //              case None =>
//  //              case Some(newMap) =>
//  //                println(f"New map")
//  //                newMap.print()
//  //            }
//  //          case None =>
//  //        }
//  //      case _ => println("invalid command, valid ones are 1,2,3")
//  //    }
//  //  }
//
//  def checkSequence(moveList: List[Char], game: Game): Option[Boolean] = {
//    @tailrec
//    def checkSequenceRec(moveList: List[Char], visitedPositions: List[((Int, Int), Option[(Int, Int)])]): Option[Boolean] = {
//      if (moveList.isEmpty)
//        Some(false)
//      else
//        game.makeAMoveForSolver(moveList.head) match {
//          case None => None
//          case Some(position) if position == game.endPosition => Some(true)
//          case Some(position) =>
//            if (visitedPositions.contains(position)) None
//            else checkSequenceRec(moveList.tail, position :: visitedPositions)
//        }
//    }
//
//    game.reset()
//    checkSequenceRec(moveList, List())
//  }
//
//  val moves = List('d', 'u', 'l', 'r')
//
//
//  def getMoveChar(move: Int): Char = {
//    move match {
//      case 0 => 'd'
//      case 1 => 'u'
//      case 2 => 'l'
//      case 3 => 'r'
//      case _ => 'x'
//    }
//  }
//
//  def getMoveInt(move: Char): Int = {
//    move match {
//      case 'd' => 0
//      case 'u' => 1
//      case 'l' => 2
//      case 'r' => 3
//      case _ => 4
//    }
//  }
//
//  def solve(game: Game): Option[List[Char]]  = {
//
//    @tailrec
//    def solver(move: Int, moveList: List[Char]): Option[List[Char]] = {
//      println("---------------------------------------")
//      println("move " + move)
//      println("movesList " + moveList.reverse.mkString("", ", ", ""))
//      if (move > 3) {
//        println("Reduce list bc of 4")
//        if (moveList.size > 1) {
//          (getMoveInt(moveList.apply(moveList.size - 2)), getMoveInt(moveList.head) + 1) match {
//            case (0, 1) => solver(2, moveList.tail)
//            case (2, 3) | (_, 4) => solver(4, moveList.tail)
//            case (3, 2) => solver(3, moveList.tail)
//            case (_, m) => solver(m, moveList.tail)
//          }
//        } else {
//          if(moveList.nonEmpty) {
//            (getMoveInt(moveList.head) + 1) match {
//              case 4 => None
//              case m => solver(m, moveList.tail)
//            }
//          }else {
//            None
//          }
//
//        }
//      }
//      else {
//        checkSequence((getMoveChar(move) :: moveList).reverse, game) match {
//          case Some(true) =>
//            println("Won")
//            Some((getMoveChar(move) :: moveList).reverse)
//          case None =>
//            println("Lost, go back")
//            if (moveList.nonEmpty) {
//              println("None: moveList.head " + getMoveInt(moveList.head))
//              (getMoveInt(moveList.head), move + 1) match {
//                case (2, 3) | (_, 4) => solver(4, moveList)
//                case (0, 1) => solver(2, moveList)
//                case (3, 2) => solver(3, moveList)
//                case (_, m) => solver(m, moveList)
//              }
//            } else {
//              solver(move + 1, moveList)
//            }
//          case Some(false) =>
//            move match {
//              case 1 => solver(1, getMoveChar(move) :: moveList)
//              case _ => solver(0, getMoveChar(move) :: moveList)
//            }
//
//        }
//      }
//    }
//
//    solver(0, List())
//  }
//
//  BloxorzMap.loadMapFromFile("src/main/maps/level07.txt") match {
//    case Some(game) => solve(game)
//    case None =>
//  }
//
//}
