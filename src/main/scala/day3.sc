import scalaz.Functor
import scalaz.Scalaz._

//Kinds and type-foo
//From LYHFGG -- Types are little labels that values carry so that we can reason about the
//values. But types have their own little labels called kinds. A kind is the type of a
//type.  Scalaz does not have a kind operator but if it did it would say:
//:kind Int
// scala.Int's kind is A.. This is a proper type
//
//:kind Option
// scala.options kind is F[+A]. * -(+)-> * -(+)-> * Its a type constructor,
// a 1st-order-kinded type
//
//:kind Either scala.util.Either's kind is F[+A1, +A2]. * -(+)-> * -(+)-> * Its a type
// constructor, a 1st-order-kinded type.
//
//:kind Equal scalaz.Equal's kind is F[A]. * -> *.  Its a type constructor,
// a 1st-order-kinded type
//
//:kind Functor scalaz.Functor's kind is X[F[A]] . (* -> *) -> *. This is a type
// constructor that takes type constructor(s): a higher-kinded type.

//Every type (like Int) that you can make a value out of is called a proper type
//and denoted with a symbol * (read "type").  This is analogous to Scala's type
//variable notation and could be written as A.

//A first-order value, or a value constructor like (_: Int) + 3 is normally
//called a function. And a first-order-kinded type is a type that
//accepts other types to create a proper type.  Some examples are Option, Either, and
//Equal.  To denote that these accept other types we use curried notation like * -> *
//and * -> * -> *.
//Option[Int] is *. Option is * -> * or F[+A] and F[+A1, +A2] respectively.



List(1,2,3).shows


//val f1 = Functor[List[Int]].lift(((_: Int) + 2))

val f2 = Functor[List].lift((_: Int) + 2)

f2(List(1,2,3))

val f3 = Functor[Option].lift((_: Int) + 2)

f3(Some(3))

f3(None)

/////////////////////////////////////////////////////////////////////////////
//    MONOIDS
// * together with 1 and ++ together with [] share some commons properties.
// The function takes two parameters and the parameters and the return value
// have the same type. There exists such a value that doesn't change other
// values when used with the binary function.
////////////////////////////////////////////////////////////////////////////
4 * 1

List(1,2,3) ++ Nil

///////////////////////////////////////////////////////////////////////////
// It doesn't matter if we do (3 * 4) * 5 or 3 8 (4 * 5). Either way the
// result is 60. The same goes for ++. This property is called associativity.
//* and ++ are associative. But - is not.
//
// A monoid is when you have an associative binary function and a value
// which acts as an identity with respect to that function.
// Monoids have a function called:
//  def zero: A
// and
//  def append(a1: A, a2: => A): A
//
// a2: => A means a2 is lazy, a called-by-name parameter that is not evaluated
// until it is actually needed.  All parameters in Haskell are like this
// but you have to make it explicit in Scala.

