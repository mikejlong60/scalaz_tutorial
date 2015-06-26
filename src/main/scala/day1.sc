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

123.310103851048515014750185710581740581085710857134058714058132.show

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












