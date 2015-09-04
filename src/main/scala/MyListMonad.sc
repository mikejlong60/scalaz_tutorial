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
def mult120(a: List[Int]) = a.map(x => x * 120)
list1 map mult120

def multAndAdd(a:Int)(b: Int) = a * b

val curMultAndAdd = multAndAdd(13)(_)
val curMultAndAdd2 = multAndAdd(1300)(_)

curMultAndAdd(3)
curMultAndAdd2(30)


case class MyNestedListFunctor2[A](list: List[A]) {
  def map(f: A => A): List[A] = innerMap(Nil, list, f)

  def innerMap(accum: List[A], whatsLeft: List[A], f: A => A): List[A] =
    whatsLeft match {
      case (x :: xs) => innerMap(f(x) :: accum, xs, f)
      case (x :: Nil) => f(x) :: accum
      case Nil => accum.reverse
    }
}
val list2 = MyNestedListFunctor2(List(List(1, 2, 3), List(4, 5, 6)))
list2 map mult12
val mult1203 = (a: List[Int]) => a.map(x => x * 1203)
list2 map mult1203
