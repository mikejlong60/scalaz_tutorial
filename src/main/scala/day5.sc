import scalaz.Functor
import scalaz.Scalaz._
import scalaz._
import Scalaz._
import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._


/////////////////////////////////////////////////////////////////
/////////////////  MONADS ///////////////////////////////////////
// Monadsare a natural extionsion of applicative functors, and
// they provide a solution to the following problem: If you
// have a value a in some context (an Option, a Future, ...),
// how do you apply it to a function that takes a normal a (Int)
// and returns a value inside a context.
/////////////////////////////////////////////////////////////////
// In scalaz the trait is called Monad.  It extends Applicative
// and bind.  Bind has the flatMap function and that's really
// important for you to understand the idea. flatMap has two symbolic
// aliases: >>= and *.

5.some flatMap {x => (x+ 2).some}
(none: Option[Int]) flatMap {x => (x + 4).some}
//versus map.  For flatMap you need to put it back in its context.
4.some map {x => (x + 10)}

Monad[Option].point("WHAT")


//Walk the Line exampe with Pierre from LHAHFGG
type Birds = Int
case class Pole(left: Birds, right: Birds) {
  def landLeft(n: Birds): Pole = copy(left = left + n)
  def landRight(n: Birds): Pole = copy(right = right + n)
}

Pole(0,0). landLeft(2)
Pole(1,2).landRight(1)
Pole(1,2).landRight(-1)
Pole(0,0).landLeft(1).landRight(4).landLeft(-1).landRight(-2)

//Above we created a failing state but the chain kept going. The two sides
//cannot be out of balance
//Let's fix it.  Fail the computation if we get more than 4 different on any side.
case class Pole2(left: Birds, right: Birds) {
  def landLeft(n: Birds): Option[Pole2] =
    if (math.abs((left + n) - right) < 4) copy (left = left + n).some
    else none
  def landRight(n: Birds): Option[Pole2] =
    if (math.abs(left - (n + right)) < 4) copy (right = right + n).some
    else none
}

Pole2(0,0).landLeft(2)
Pole2(0,3).landLeft(10)

//You can chain
val p3 = Monad[Option].point(Pole2(0,0)) flatMap {_.landRight(2)} flatMap {_.landLeft(200)} flatMap {_.landRight(200)}

//Or use the >>= operator
val p4 = Monad[Option].point(Pole2(0,0)) >>= {_.landRight(2)} >>= {_.landLeft(200)} >>= {_.landRight(200)}







