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


