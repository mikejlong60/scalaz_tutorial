import scalaz._
import Scalaz._

/////////////////////////////////////
//Equal typeclass
////////////////////////////////////
1 === 1

//1 === "fpp" //-- this fails to compile w/ scalaz ===


//Scalaz implements 3 new operators. ===, =/=, and assert_. They
//differ from scala in that they fail if the types are wrong.

1.some === 1.some

1.some =/= 2.some

//1 assert_=== 2


//////////////////////////////////////
//Order typeclass
//////////////////////////////////////
1 > 2.0  //-- regular scala

//1 gt 2.0 scalaz equivalent won't compile.

1 gt 2
2 lte 2
3 gte 2
1.0 max 2.0
//1 max 2.0 -- won't compile

///////////////////////////////////////////////////////////////
// Show typeclass
// Members of Show can be interpreted as a string.
// cord(show) is a purely functional data structure for
// generating long strings.
///////////////////////////////////////////////////////////////
3.show
3.shows
"hello".println
123.310103851048515014750185710581740581085710857134058345714058132.show
///////////////////////////////////////////////////
// Read is the opposite typeclass of Show. It takes a String
// and returns a type that is a member of Read.
// There is no equivalent in Scalaz.
//////////////////////////////////////////////////



//////////////////////////////////////////////////
// Enum is an ordered list, has predecessors and
// successors.
// Instead of the standard to, Enum enables |->
// that returns a List by declaring pred and succ
// methods on top of the Order typeclass. Includes
// other operations like -+-, ---, from, fromStep,
// pred, predx, succ, succx, |-->, |->, |==>, and |=>.
// All these step forward and backward through ranges.
//////////////////////////////////////////////////
'a' to 'e' //regular scala

'a' |-> 'e'

'a' |=> 'e'  // Makes an EphemeralStream, a lazy version of the enum.
//////////////////////////////////////////////////
//  Bounded type class in Haskell is also covered
// by the Scalaz type class.
// Bounded members have an upper and a lower bound.
/////////////////////////////////////////////////
implicitly[Enum[Int]].max

/////////////////////////////////////////////////
// Type Classes
//
// In Haskell I can make an ADT: data TrafficLight = Red | Yellow | Green
// In Scala I have to do:
////////////////////////////////////////////////
case class TrafficLight(name: String)
val red = TrafficLight("red")
val yellow = TrafficLight("yellow")
val green = TrafficLight("green")
implicit val TrafficLightEqual: Equal[TrafficLight] = Equal.equal(_ == _)
red == green

/////////////////////////////////////////////////
// Truthy typeclass
//

trait CanTruthy[A] { self =>
  /** @return true, if 'a' is truthy. */
  def truthys(a: A): Boolean
}

object CanTruthy {
  def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev
  def truthys[A](f: A => Boolean): CanTruthy[A] =  new CanTruthy[A] {
    def truthys(a: A): Boolean = f(a)
  }
}
trait CanTruthyOps[A] {
  def self: A
  implicit def F: CanTruthy[A]
  final def truthy: Boolean = F.truthys(self)
}
object ToCanIsTruthyOps {
  implicit def toCanIsTruthyOps[A](v: A)(implicit ev: CanTruthy[A]) =
    new CanTruthyOps[A] {
      def self = v
      implicit def F: CanTruthy[A] = ev
    }
}

import ToCanIsTruthyOps._
implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.truthys({
  case 0 =>  false
  case _ => true
})

0.truthy
10.truthy

implicit def listCanTruthy[A]: CanTruthy[List[A]] = CanTruthy.truthys({
  case Nil => false
  case _ => true
})

List().truthy
List(1,2,3).truthy

implicit val nilCanTruthy: CanTruthy[scala.collection.immutable.Nil.type] = CanTruthy.truthys(_ => false)

Nil.truthy

implicit val booleanCanTruthy: CanTruthy[Boolean] = CanTruthy.truthys(identity)
true.truthy
false.truthy


