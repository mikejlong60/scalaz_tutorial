//import scalaz.Alpha.A

import scalaz.Alpha.A
import scalaz.{Applicative, Apply, Tags, Functor}
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
//Option as Apply.  You can use <*> on option.

9.some <*> {(_: Int) + 3}.some

// *> and <* are variations that return the rhs and lhs
9.some <* 2.some

none <* 2.some

none *> 3.some

9.some *> 3.some

9.some <* {(_: Int) + 4}.some


//Scalaz 7 has a new notation that extracts values from containers and applies them
//to a single function.
val y = ^(3.some, 5.some) {_ + _}

val y1 = ^(3.some, none[Int]) {_ + _}


//Lists as Apply. You can use <*> and |@| on lists.
val z1 = List(1,2,3) <*> List((_: Int) * 0, (_: Int) + 100, (x: Int) => x * x)

val z2 = List(3,4) <*> {List(1,2) <*> List({(_: Int) + (_: Int)}.curried, {(_: Int) * (_: Int)}.curried)}

val z3 = (List("ha", "heh", "hmm") |@| List("?", "!", ".")) {_ + _}

val z4 = (List(1, 2, 3) |@| List(100, 200, 300)) {_ + _}

///Zip lists -- In Haskell [(+3),(*2)] <*> [1,2] works in such a way that
//the first function in the left list gets applied to the first value in the
//right one, the second function gets applied to the second value and so on.
//That would result in a list with two values, namely [4,4]. Look at is as
//[1 + 3, 2 * 2]

//You can't do this easily in Scalaz.

val a1 = streamZipApplicative.ap(Tags.Zip(Stream(1,2))) (Tags.Zip(Stream({(_: Int) + 3}, {(_: Int) * 2})))




//Control.applicative has a function called liftA2 which has a type of
//liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c

val b1 = Apply[Option].lift2((_: Int) :: (_: List[Int]))

b1(3.some, List(4).some)

//Here we implement a function that takes a list of applicatives and returns
//an applicative that has a list as its result value.

def sequenceA[F[_]: Applicative, A](li: List[F[A]]): F[List[A]] = li match {
  case Nil => (Nil: List[A]).point[F]
  case x :: xs => (x |@| sequenceA(xs)) {_ :: _}
}

sequenceA(List(1.some, 2.some))

sequenceA(List(3.some, none, 1.some))

sequenceA(List(List(1,2,3), List(4,5,6)))

//For Function1 with Int fixed we have to do something wierd which I do not yet understand.
type Function1Int[A] = ({type l[A]=Function1[Int, A]})#l[A]

val c2 = sequenceA(List((_: Int) + 3, (_: Int) + 2, (_: Int) + 1): List[Function1Int[Int]])


c2(3)

