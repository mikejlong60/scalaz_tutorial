def sum(xs: List[Int]): Int = xs.foldLeft(0) { _ + _ }

sum(List(1,2,3,4,15))



trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}


object IntMonoid extends Monoid[Int] {
  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}

def sumM1(xs: List[Int]): Int = xs.foldLeft(IntMonoid.mzero)(IntMonoid.mappend)

sumM1(List(1,2,3,4,5))


def sumM2(xs: List[Int], m: Monoid[Int]): Int = xs.foldLeft(m.mzero)(m.mappend)

sumM2(List(1,2,3,4,500), IntMonoid)

def sumM3[A](xs: List[A], m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)

sumM3(List(1,2,3,4,500), IntMonoid)

implicit val intMonoid = IntMonoid

def sumM4[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)

sumM4(List(1,2,3,4,500))




