case class MyNestedListFunctor(list: List[List[Int]]) {
  def map(f: List[Int] => List[Int]) = list match {
    case (x :: xs) => innerMap(Nil, list, f)
  }

  def innerMap(accum: List[List[Int]], whatsLeft: List[List[Int]], f: List[Int] => List[Int]): List[List[Int]] = whatsLeft match {
    case (x :: xs) => innerMap(f(x) :: xs, xs, f)
    case (x :: Nil) => f(x) :: accum
    case Nil => Nil
  }
}

val fred3 = MyNestedListFunctor(List(List(1, 2, 3), List(4, 5, 6)))
val fred4 = fred3 map { subList: List[Int] =>
  {
    println(subList)
    val y = subList.map(x => x + 12)
    //val y:List[Int] = subList.map { x: Int =>
    //  {
    //    println(x)
    //    x + 12
    //  }
    //  y
    y
    }
  }

