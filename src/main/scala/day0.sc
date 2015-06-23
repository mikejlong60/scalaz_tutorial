def sum(xs: List[Int]): Int = xs.foldLeft(0) { _ + _ }

sum(List(1,2,3,4,15))

object IntMonoid {
  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}

def sumM(xs: List[Int]): Int = xs.foldLeft(IntMonoid.mzero)(IntMonoid.mappend)


sumM(List(1,2,3,4,5))
