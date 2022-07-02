class Person(val name: String, val age: Int) {

}

object tmp extends App {

  val people: Array[Person] = Array(Person("1", 15), Person("2", 19), Person("3", 15), Person("4", 15))
  val (minors, adults) = people partition (_.age < 18)

  println(minors.map(p => p.name).mkString(","))

  for (i <- 0 to 10 by 2) {
    print(i)
    print(",")
  }
  println()

  for (i <- 0 until 10 by 2) {
    print(i)
    print(",")
  }
  println()

  val arr = for {i <- 0 until 10 by 2} yield i
  println(arr.mkString(","))

  def f: Int = try return 1 finally return 2 // 2

  def g: Int = try 1 finally 2 // 1

  print(f, g)
  println()

  def isIntIntMap(x: Any) = x match {
    case m: Map[Int, Int] => true
    case _ => false
  }

  println(isIntIntMap(Map(1 -> 1)))
  println(isIntIntMap(Map("abc" -> "abc")))


  def isIntIntArray(x: Any) = x match {
    case m: Array[Int] => true
    case _ => false
  }

  println(isIntIntArray(Array(1,2)))
  println(isIntIntArray(Array("abs","asd")))
}
