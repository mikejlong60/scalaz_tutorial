import scalaz.Functor
import scalaz.Scalaz._
import scalaz._
import Scalaz._
import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._


/////////////////////////////////////////////////////////////////
/////////////////  MONADS ///////////////////////////////////////
// Monads are a natural extension of applicative functors, and
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


//Walk the Line example with Pierre from LHAHFGG
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
  def banana: Option[Pole2] = none
}

Pole2(0,0).landLeft(2)
Pole2(0,3).landLeft(10)

//You can chain
val p3 = Monad[Option].point(Pole2(0,0)) flatMap {_.landRight(2)} flatMap {_.landLeft(200)} flatMap {_.landRight(200)}

//Or use the >>= operator(same as flatmap)
val p4 = Monad[Option].point(Pole2(0,0)) >>= {_.landRight(2)} >>= {_.landLeft(200)} >>= {_.landRight(200)}

// You can force an error by throwing a banana onto the wire.
val p5 = Monad[Option].point(Pole2(0,0)) >>= {_.landRight(2)} >>= {_.banana} >>= {_.landLeft(2)} >>= {_.landRight(2)}


//Instead of making functions that ignore their input and just return a predetermined
//monadic value you can just use the >>=(flatmap) function.
val p6 = Monad[Option].point(Pole2(0,0)).>>= {_.landRight(2)} >> (none: Option[Pole2]) >>= {_.landLeft(2)} >>= {_.landRight(2)}


//Do notation - Monads are so useful that they get their own special syntax called do notation.
//Scala has for syntax which does the same thing.  In a for expression every line that
//isn't a let is a monadic value.
for {
  x <- Monad[Option].point(Pole2(0,0))
  y <- x.landLeft(2)
  z <- y.landRight(2)
} yield z

//Throw a banana in for(do) notation.
for {
  x <- Monad[Option].point(Pole2(0,0))
  y <- x.banana
  z <- y.landRight(2)
} yield z

//Or in >> notation
for {
  x <- Monad[Option].point(Pole2(0,0))
  y <- (none: Option[Pole2])
  z <- y.landRight(2)
} yield z


//In do notation you can utilize pattern matching when you bind monadic values to
//names.  You can also do this in let expressions and in function parameters, but
//I don't know much about either of those.
def justH: Option[Char] =
  for {
    (x :: xs) <- "hello".toList.some
  } yield x

justH

def justEllo: Option[List[Char]] =
  for {
    (x :: xs) <- "hello".toList.some
  } yield xs

justEllo

//When pattern matching fails in a do expression, scalaz calls the fail function.
//That function is a member of the Monad type class. That function makes a pattern
//match failure result in the failure of the current context's Monad instead of
//making the program crash. In JVM terminology it must be catching the exception
//and wrapping it in something else rather than letting the failure propagate
//up the call stack.
def wopwop: Option[Char] =
  for {
    (x :: xs) <- "".toList.some
  } yield x
wopwop



//The List monad -- a value like [3,8,9] contains several results. Another way
//to view it is as one value that has many values at the same time, like in a
//quantum computer where a bit can be both on and off at the same time. Lists
//are applicative functors allows you to express this non-determinism.
^(List(1,2,3), List(10,100,1000)) {_ * _}

//Now we will feed a non-deterministic value to a function
List(3,4,5) >>= {x => List(x, -x)}

//So here the List context is a mathematical value that could have multiple solutions.
//Other than that using for notation on lists is just like Scala.
for {
  n <- List(1,2)
  ch <- List('a', 'c','d')
} yield (n, ch)

//MonadPlus and the guard function.   Scala's for notation allows filtering.
//The MonadPlus typeclass is for monads that can also act like monoids.
for {
  x <- 1 to 50 if x.shows contains '7'
} yield x


//A knight's quest. A problem that lends itself to being solved with non-determinism
//is trying to find out if a single night on a chessboard can reach a certain position
//in three moves.
case class KnightPos(c: Int, r: Int) {
  def move: List[KnightPos] =
    for {
        KnightPos(c2, r2) <- List(KnightPos(c + 2, r - 1), KnightPos(c + 2, r + 1),
          KnightPos(c - 2, r - 1), KnightPos(c - 2, r + 1),
          KnightPos(c + 1, r - 2), KnightPos(c + 1, r + 2),
          KnightPos(c - 1, r - 2), KnightPos(c - 1, r + 2)) if (
      ((1 to 8) contains c2) && ((1 to 8) contains r2))
    } yield KnightPos(c2, r2)

  def in3: List[KnightPos] =
    for {
      first <- move
      second <- first.move
      third <- second.move
    } yield third

  def canReachIn3(end: KnightPos): Boolean = in3 contains end
}

KnightPos(6,2).move
KnightPos(6, 2).in3
KnightPos(6, 2) canReachIn3 KnightPos(6, 1)
KnightPos(6, 2) canReachIn3 KnightPos(7, 3)

//Monad Laws
//Law 1 - Left Identity -- If you have a value that you put in a default context with return
// and then feed it to a function by using >>=, its the same as just taking the value
// and applying the function to it.
//
// In Scala here is an example. Remember that in Scalaz point is the same as return
// in Haskell.
(Monad[Option].point(3) >>= { x => (x + 100000).some }) assert_=== 3 |> { x => (x + 100000).some}

//Law 2 - Right Identity -- If you have a monadic value and we use >>= to feed it to return,
//the result is the original monadic value.
("move on up".some flatMap {Monad[Option].point(_)}) assert_=== "move on up".some


//Law 3 - Associativity -- When you have a chain of monadic function applications
//with >>=, it doesn't matter how they are nested.
Monad[Option].point(Pole2(0,0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.banana} >>= {_.landRight(2)}

val xx = Monad[Option].point(Pole2(0,0)) >>= {x =>
  x.landRight(2) >>= { y =>
  //y.banana >>= {z =>
  y.landLeft(2) >>= {z =>
  z.landRight(2)
  }}}


///BIG TAKEAWAY --- A Monad has flatMap(aliased as >>=).
