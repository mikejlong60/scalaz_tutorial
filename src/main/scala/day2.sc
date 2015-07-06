import scalaz.Functor
import scalaz.syntax.Ops
import scalaz.Scalaz._

trait Functor[F[_]] { self =>
  /** Lift 'f' into 'F' and apply it to 'F[A]'. */
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait FunctorOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Functor[F]

  import scalaz.Leibniz.===

  final def map[B](f: A => B): F[B] = {
    println(1)
    val result = F.map(self)(f)
    println(result)
    result
  }
}

List("1","2", "3") map { _ + 3}

(1, 2, 3) map {_ + 8}

def fred = ((x: Int) => x + 1) map {_ * 7}
fred(3)


List(1,2,3) map { _ + 3}

def alift = Functor[List].lift {(_: Int) * 2}

alift(List(1))

//What happens when we map a function that takes two parameters over a functor?  Before
//all our functions took one parameter.
//List(2,4,6,8) map {(_: Int) * (_: Int)} //You get an error that says its expecting a
//function that takes one parameter and you gave it a function that takes two parameters.

//We have to curry it with this special form
def applicFunctor = List(1,2,3,4) map {(_: Int) * (_: Int)}.curried
applicFunctor map {_ (9)}



//The Scalaz class called Applicative extends another typeclass called Apply and
//introduces point and its alias pure.  pure should take a value of any type and
//return an applicative value with that value inside it.  It takes a value and puts
//it in a minimal context that still yields a value.
1.point[List]
1.point[Option]
1.point[Option] map {(x: Int) => x + 2}
//or
1.point[Option] map {_ + 2}

1.point[List] map {(x: Int) => x + 2}
//or
1.point[List] map {_ + 2}

//Think of Apply or <*> in Haskell as a beefed-up fmap. Whereas fmap takes a function
//and a functor and applies the function inside the functor value, <*> takes a
//functor that has a function in it and another functor and extracts that function
//from the first functor and then maps it over the second one.

9.some <*> {(_: Int) + 3}.some

// *> and <* are variations that return the rhs and lhs
9.some <* 2.some

none <* 2.some

none *> 3.some

9.some *> 3.some

9.some *> {(_: Int) + 3}.some















