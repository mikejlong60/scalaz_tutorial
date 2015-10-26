import scalaz._
import Scalaz._

//Oragami programming. This means fold and unfold.  Folds and unfolds
//are the natural patterns of computation over recursive data types.
//Folds consume data structures and unfolds generate data structures.

val dude  = DList.unfoldr (10, {(x: Int) => if (x == 0) none else (x, x-1).some })
dude.toList
