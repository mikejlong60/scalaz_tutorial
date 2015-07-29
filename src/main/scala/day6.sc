import scalaz.Scalaz._
import scalaz._
import Scalaz._
import scalaz.Functor
import scalaz.Scalaz._
import scalaz._
import Scalaz._
import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._
import scalaz.syntax.ToIdOps
import scalaz.syntax.ToTreeOps
import scalaz.syntax.ToValidationOps
import scalaz.syntax.ToWriterOps
import scalaz.syntax._

val foo = for {
  x <- 3.some
  y <- "?".some
} yield x.shows + y

// The Writer monad -- The Maybe monad is for values with an added context for failure
//and the List monad is for non-deterministic values. Writer is for values that have
//another value attached that acts like a logger.
def isBigGang(x: Int): (Boolean, String) = (x > 9, "Compared gang size to 9.")

implicit class PairOps[A, B: Monoid](pair: (A, B)) {
  def applyLog[C](f: A => (C, B)):(C, B) = {
    val (x, log) = pair
    println(x)
    println(log)
    val (y, newlog) = f(x)
    (y, log |+| newlog)
  }
}

(3, "smallish gang") applyLog isBigGang


//To attach a monoid to a value you need to put them together in a tuple. The
//Writer w a type is just a newtype wrapper for this.
//type Writer[+W, +A] = WriterT[Id, W, A]
//sealed trait WriterT[F[+_], +W, +A] { self =>
//  val run: F[(W, A)]
//
//  def written(implicit F: Functor[F]): F[W] =
//    F.map(run)(_._1)
//
//  def value(implicit F: Functor[F]): F[A] =
//    F.map(run)(_._2)
//}

4.set("Smallish Gang")

//The following operators are supported by all data types enabled by "import Scalaz._"
trait toDataOps extends ToIdOps with ToTreeOps with ToWriterOps with ToValidationOps with ToReducerOps with ToKleisliOps

//trait WriterV[A] extends Ops[A] {
//  def set[W](w:W): Writer[W,A] = WriterT.writer(w -> self)
//
//  def tell: Writer[A, Unit] = WriterT.tell(self)
//
//}

//The above methods are injected to al types so we can use them to create Writers.
3.set("something")

Monoid[Vector[String]]


def gcd(a: Int, b: Int): Writer[Vector[String], Int] =
  if (b == 0) for {
        _ <- Vector("Finished with " + a.shows).tell
      } yield a
  else for {
        result <- gcd(b, a % b)
        _ <- Vector(a.shows + " mod " + b.shows + " = " + (a % b).shows).tell
        } yield result


gcd(8, 3).run

//import scalaz.syntax.monoid
//
//def vectorFinalCountDown(x: Int): Writer[Vector[String], Unit] = {
//  import annotation.tailrec
//  @tailrec def doFinalCountDown(x: Int, w: Writer[Vector[String], Unit]): Writer[Vector[String], Unit] = x match {
//    case 0 => w >>= { _  => Vector("0").tell}
//    case x => doFinalCountDown(x - 1, w >>= {_ => Vector(x.shows).tell})
//  }
//}

///////////////////////////////////////////////////////
// Reader -- the function type (->) r is an instance of Functor.
// That means it has map.  Remember that mapping over a function
// Functor is just function composition.
/////////////////////////////////////////////////////

val f: Int => Int = (_: Int) * 5
//or
val f1: ((Int, Int) => (Int, Int)) = (s: Int, x: Int) => (s -> x)

val g = (_: Int) + 3
(g map f)(8) // == 55 = (8 + 3) * 5

//Functions are applicative functors. They allow us to
//operate on the eventual results of functions as if
//we already had their results.

val f2 = ({(_: Int) * 2} |@| {(_: Int) + 10}) {_ + _}

f2(3) // == 19 = (3 * 2) + (3 + 10)

//Not only is the function type (->) r a functor but its also an applicative
//functor and a monad. A function in this case is a value with a context
//just like other functors(Maybe, Option...).

val addStuff: Int => Int = for {
        a <- (_: Int) * 2
        b <- (_: Int) + 10
    } yield a + b

addStuff(3)

//In this case both (*2) and (+10) get applied to the number 3.