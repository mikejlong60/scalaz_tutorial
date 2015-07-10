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
//and denoted with a symbol * (read "type").  This is analagous to Scala's type
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



