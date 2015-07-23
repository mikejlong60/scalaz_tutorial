import scalaz.Functor
import scalaz.Scalaz._
import scalaz._
import Scalaz._
import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._

///////////////////////////////////////////////////////////////////////
// Functor Laws:
//
// All functors are expected to exhibit certain kinds of functor-like
// properties and behaviors. The first functor law states that if we
// map the ID function over a functor, the functor that we get back
// should be the same as the original functor.
//
// In other words:
val x = List(1,2,3) map {identity}
x assert_=== List(1,2,3)

/////////////////////////////////////////////////////////////////////
// The second functor law states that composing two functions and then
// mapping the resulting function over a functor should be the same
// as first mapping one function over the functor and then mapping
// the other one.
//
// In other words:
val y1 = (List(1,2,3) map {{(_: Int) * 3} map {(_: Int) + 1}})
val y2 = (List(1,2,3) map {(_: Int) * 3} map {(_: Int) + 1})

y1 assert_=== y2


functor.laws[List].check


//This guy is not a functor because he breaks the functor laws:
sealed trait COption[+A] {}
case class CSome[A](counter: Int, a: A) extends COption[A]
case object CNone extends COption[Nothing]

implicit def coptionEqual[A]: Equal[COption[A]] = Equal.equalA
implicit val coptionFunctor = new Functor[COption] {
  def map[A, B](fa: COption[A])(f: A => B): COption[B] = fa match {
    case CNone => CNone
    case CSome(c, a) => CSome(c + 1, f(a))
  }
}

//import scalacheck.{Gen, Arbitrary}
//functor.laws[COption].check

///////////////////////////////////////////////////////////////////////
// Semigroup Laws
// -- closure: `A a, b in F, append(a, b)` is also in `F`. This is
//        enforced by the type system
// -- associativity - If all are Functors append(append(a,b), c) = append(a, append(b, c))
//////////////////////////////////////////////////////////////////////
1 * (2 * 3) assert_=== (1 * 2) * 3

//////////////////////////////////////////////////////////////////////
// Monoid Laws
//    Monoid instances must satisfy Semigroup laws plus 2 additional
//    laws: left identity and right identity. They are as follows:
//      left --- forall a append(zero, a) == a
//      right -- forall a append(a, zero) == a
/////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////
// Foldable
// There are many data structures that work with folds.  Foldable type
// class exists for thing that can be folded.
//
// Equivalent in Scalaz is called Foldable. Here it is:

trait Foldableabc[F[_]] { self =>
  /** Map each element of the structure to a scalaz.Monoid and combne the results. */
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B

  /** Right-associative fold of a structure */
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

}

List(1,2,3).foldRight(1) {_ * _}

9.some.foldLeft(2) {_ + _}

List(1,2,3).foldLeft(1) {_ - _}

List(1,2,3) foldMap {identity}

