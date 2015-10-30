import scalaz.Alpha.{F, A}
import scalaz.Leibniz.===

//import scalaz.Leibniz.===
import scalaz._
import Scalaz._

//Oragami programming. This means fold and unfold.  Folds and unfolds
//are the natural patterns of computation over recursive data types.
//Folds consume data structures and unfolds generate data structures.
val dude  = DList.unfoldr (10, {(x: Int) => if (x == 0) none else (x, x-1).some })
dude.toList
val stream = unfold(100) {(x) => if (x ==0) none else (x, x-1).some}
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

//The essence of the iterator pattern. Scalaz implements applicative t turn any monoid
//into an applicative.
Monoid[Int].applicative.ap2(1,1)(0)
Monoid[List[Int]].applicative.ap2(List(1), List(2))(Nil)


//Now we make a product of List and Option
Applicative[List].product[Option]
Applicative[List].product[Option].point(1)


//Product is implemented as a Tuple2. Here I use Applicative style to append them.
((List(1), 1.some) |@| (List(1), 1.some)) {_ |+| _}
((List(1), 1.success[String]) |@| (List(1), "boom".failure[Int])) {_ |@| _}


//You can compose applicatives and it remains applicative.
//Traversal involves iterating over the elements of a data structure in the style of
//a map but interpreting certain functions idiomatically.
//Corresponding type class in Scalaz 7 is Traverse.

//Here's how to use it in a list.
List(1,2,3) traverse {x => (x > 0) option (x + 1)}
List(1,2,0) traverse {x => (x > 0) option (x + 1)}

//The option operator is injected to Boolean, which expands (x > 0) option (x + 1)
//to if (x > 0) Some(x + 1) else None

//In the case of a monadic applicative functor it feels similar to flatMap.
//For a monoidal applicative functor, traversal accumulates values. The function
//reduce performs the accumulation, given an argument that assigns a value to
//each element.
Monoid[Int].applicative.traverse(List(1,2,3)) {_ + 1}

//The generic traverse operation is parametrised along two further dimensions:
//the data type being traversed and the applicative functor in which the traversal
//is interpreted.
def contents[F[_]: Traverse, A](f: F[A]): List[A] = Monoid[List[A]].applicative.traverse(f) {List(_)}
contents(List(1,2,3))
contents(NonEmptyList(1,2,3))
val tree: Tree[Char] = 'P'.node('O'.leaf, 'L'.leaf)
contents(tree)

//The upshot of above is that you can take any data structure that supports Traverse and turn
//it into a List.

//The "identity idiom" is the Id monad in Scalaz.
def shape[F[_]: Traverse, A](f: F[A]): F[Unit] = f traverse {_ => ((): Id[Unit])}

shape(List(1,2,3))
shape(tree).drawTree

//The next pair of traversals show two important aspects of iterations: mapping and accumulation.
def decompose[F[_]: Traverse, A](f: F[A]) = (shape(f), contents(f))
decompose(tree)

//Notice how decompose is looping the tree structure twice. Remember that a product
//of two applicatives is also an applicative.
decompose(List(1,2,3,4))

//*** Sequence ***
//Sequence introduces a method called sequence. It evaluates each action in the sequence from
//left to right and collects the results.
//def sequence2[G[_], B](implicit ev: A === G[B], G: Applicative[G]): G[F[B]] = {
//  val fgb: F[G[B]] = ev.subst[F](self)
//  F.sequence(fgb)
//}

List(1.some, 2.some).sequence
List(1.some, 2.some, none).sequence

//This works for other data structures as well.






