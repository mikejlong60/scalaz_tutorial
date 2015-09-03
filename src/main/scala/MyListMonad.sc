case class MyNestedListFunctor(list: List[List[Int]]) {
  def map(f: List[Int] => List[Int]): List[List[Int]] = innerMap(Nil, list, f)

  def innerMap(accum: List[List[Int]], whatsLeft: List[List[Int]], f: List[Int] => List[Int]): List[List[Int]] =
    whatsLeft match {
      case (x :: xs) => innerMap(f(x) :: accum, xs, f)
      case (x :: Nil) => f(x) :: accum
      case Nil => accum.reverse
    }
}

val list1 = MyNestedListFunctor(List(List(1, 2, 3), List(4, 5, 6)))
val add12 = (a: List[Int]) => a.map(x => x + 12)
list1 map add12
val emptyList = MyNestedListFunctor(List(List.empty[Int]))
emptyList map add12
val mult12 = (a: List[Int]) => a.map(x => x * 12)
list1 map mult12