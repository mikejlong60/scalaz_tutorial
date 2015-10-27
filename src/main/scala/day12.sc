import scalaz._
import Scalaz._

//Oragami programming. This means fold and unfold.  Folds and unfolds
//are the natural patterns of computation over recursive data types.
//Folds consume data structures and unfolds generate data structures.

val dude  = DList.unfoldr (10, {(x: Int) => if (x == 0) none else (x, x-1).some })
dude.toList

val stream = unfold(100000) {(x) => if (x ==0) none else (x, x-1).some}
stream.toList

//Here is an implementation of stream sort that uses folds
def minimumS[A: Order](stream: Stream[A]) = stream match {
  case x #:: xs => xs.foldLeft(x) { _ min _}
}

def deleteS[A: Equal](y: A, stream: Stream[A]): Stream[A] = (y, stream) match {
  case (_, Stream()) => Stream()
  case (y, x #:: xs) =>
      if (y === x) xs
      else x #:: deleteS(y, xs)
}

def delim[A: Order](stream: Stream[A]): Option[(A, Stream[A])] = stream match {
  case Stream() => none
  case xs =>
    val y = minimumS(xs)
    (y, deleteS(y, xs)).some
}

def ssort[A: Order](stream: Stream[A]): Stream[A] = unfold(stream){delim[A]}

ssort(stream).toList