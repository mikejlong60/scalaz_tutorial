def sum(xs: List[Int]): Int = xs.foldLeft(0) {
  _ + _
}

sum(List(1, 2, 3, 4, 15))


trait Monoid[A] {
  def mappend(a1: A, a2: A): A

  def mzero: A
}


object AddIntMonoid extends Monoid[Int] {
  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}

object MultiIntMonoid extends Monoid[Int] {
  def mappend(a: Int, b: Int): Int = a * b

  def mzero: Int = 1
}

object StringMonoid extends Monoid[String] {
  def mappend(a: String, b: String): String = a + b

  def mzero: String = ""
}

//def sumM1(xs: List[Int]): Int =
//  xs.foldLeft(AddIntMonoid.mzero)(AddIntMonoid.mappend)
//
//sumM1(List(1,2,3,4,5))
//
//
//def sumM2(xs: List[Int], m: Monoid[Int]): Int =
//  xs.foldLeft(m.mzero)(m.mappend)
//
//sumM2(List(1,2,3,4,500), AddIntMonoid)
//
//def sumM3[A](xs: List[A], m: Monoid[A]): A =
//  xs.foldLeft(m.mzero)(m.mappend)
//
//sumM3(List(1,2,3,4,500), AddIntMonoid)
//
def sumM4[A](xs: List[A])(m: Monoid[A]): A =
  xs.foldLeft(m.mzero)(m.mappend)

//sumM4(List(1,2,3,4,500))(AddIntMonoid)

//sumM4(List("1","2","3","4","500"))(StringMonoid)
//
//sumM4(List())(AddIntMonoid)

sumM4(List(1, 2, 3, 4, 500))(MultiIntMonoid)

sumM4(List())(MultiIntMonoid)

//implicit val IntMonoid2: Monoid[Int] = new Monoid[Int] {
//  def mappend(a: Int, b: Int): Int = a + b
//  def mzero: Int = 0
//}

implicit val multMonoid = MultiIntMonoid
implicit val stringMonoid = StringMonoid


object FoldLeftList {
  def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) =
    xs.foldLeft(b)(f)
}

def sum2[A: Monoid](xs: List[A]): A = {
  val m = implicitly[Monoid[A]]
  FoldLeftList.foldLeft(xs, m.mzero, m.mappend)
}

sum2(List(1, 2, 3, 40))



sum2(List(1, 2, 300, 404))


trait FoldLeft[F[_]] {
  def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List] {
    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
  }
}

def sum3[M[_] : FoldLeft, A: Monoid](xs: M[A]): A = {
  val m = implicitly[Monoid[A]]
  val fl = implicitly[FoldLeft[M]]
  fl.foldLeft(xs, m.mzero, m.mappend)
}

sum3(List(1,2,30))
sum3(List("a","mike","="))


def plus[A: Monoid](a: A, b: A): A = implicitly[Monoid[A]].mappend(a, b)
plus(12,12)


trait MonoidOp[A] {
  val F: Monoid[A]
  val value: A
  def |+| (a2: A) = F.mappend(value, a2)
}

implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
  val F = implicitly[Monoid[A]]
  val value = a
}

3 |+| 4

import scalaz.syntax.std.all._

1.some

2.some

2.some.getOrElse(3)


(3 > 12) ? true | false

if (1 > 2) 1 else 2






























