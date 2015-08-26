import scalaz._
import Scalaz._


//The focus of this chapter ismonadic functions, functions that operate on
// monadic values or return monadic values as their results.
//
//In Scalaz Monad extends Appicative so all monads are functors, meaning
//that they have a map or <*> operator.

//Monads also have a join method, a way of flattening values. This property
//is unique to monads. Haskell calls this function join.   Scalaz has
//join and its alias u(Greek u).

(Some(9.some): Option[Option[Int]]).join
(Some(none): Option[Option[Int]]).join

List(List(0,1,2,3,4), List(5,6,7), List(8,9)).join

9.right[String].right[String].join
"boom".left[Int].right[String].join


//The filterM function from Control.MOnad returns a monadic value
//whose result is a Bool.  Its just a monadic version of filter.

List(1,2,3) filterM {x => List(true, false)}
Vector(1,2,3) filterM {x => Vector(true, false)}

//Likewise, foldLeftM is a monadic version of foldLeft.
//Also have foldRightM.
def binSmalls(acc: Int, x: Int): Option[Int] = {
  if (x > 9) (none: Option[Int])
  else (acc + x).some
}

List(2,8,3,1).foldLeftM(0) {binSmalls}

List(2,80,3,1).foldLeftM(0) {binSmalls}


//Making a safe RPN calculator
def foldingFunction(list: List[Double], next: String): Option[List[Double]] = (list, next) match {
  case (x :: y :: ys, "*") => ((x * y) :: ys).point[Option]
  case (x :: y :: ys, "+") => ((x + y) :: ys).point[Option]
  case (x :: y :: ys, "-") => ((y - x) :: ys).point[Option]
  case (xs, numString) => numString.parseInt.toOption map { _ :: xs}
}

def solveRPN(s: String): Option[Double] = for {
  List(x) <- s.split(' ').toList.foldLeftM(Nil: List[Double]) {foldingFunction}
} yield x
solveRPN("1 2 * 4 +")



("10 4 30 + 2 * -").split(' ').toList


//Composing monadic functions --- Recall from the monad laws that the <=< function
// is function composition. But instead of working for normal functions like
// A => B, it works for A => M[B] where M is a monad.
//In Scalaz there is a wrapper for functions of the type called Kleisli.
//The Kliesli trait has 'compose' and 'andThen'. The former is aliased as <=<
//and the later as ">=>".

val f = Kleisli { (x: Int) => (x + 1).some}
val g = Kleisli { (x: Int) => (x * 100).some}
4.some >>= (f <=< g)//f compose g

4.some >>= (f >=> g)// f andThen g

//Scalaz defines Reader as a special case of Kleisli
//Here  I rewrite the reader example from day 6
//using Kleisli.
val addStuff: Reader[Int, Int] = for {
  a <- Reader {(_:Int) * 2}
  b <- Reader {(_:Int) + 10}
} yield a + b

addStuff(3)
//Note here how you are using function as a monad {(_:Int) + 10}