import scalaz._
import Scalaz._


//This chapter covers other Monads such as Tree. Due to the equality
//of case classes tree nodes that have the same type and value are equal.

def freeTree: Tree[Char] =
  'P'.node(
    'O'.node(
      'L'.node('M'.leaf, 'T'.leaf),
      'Y'.node('S'.leaf, 'A'.leaf)),
    'L'.node(
      'W'.node('C'.leaf, 'R'.leaf),
      'A'.node('A'.leaf, 'C'.leaf)))

//def changeToP(tree: Tree[Char]): Tree[Char] = tree match {
//  case Tree.Node(x, Stream(
//    'L', Tree.Node(y, Stream(
//      Tree.Node(_, Stream(m, n)), r)))) =>
//    x.node(1, y.node('P'.node(m, n), r))
//}

//This is too hard to implement, plus I can't get it to compile so
//I am moving on to a data structure called a zipper. With a zipper
//you focus on a part of a data structure and its surroundings.  This
//structure resembles a zipper on a pair of pants.
//The zipper for Tree in Scalaz is called TreeLoc.  TreeLoc implements
//methods that allow you to move the focus around like if you are
//using the DOM.


val zipper = freeTree.loc
zipper.getChild(1)// >>= {_.getChild(1)}


//Zippers exist for many data structures. In the following example I use them
//to focus on a sublist of a list.  Scalaz provides a Zipper for Stream. Since
//Haskell is lazy Haskell's List is really like Scala's Stream.
Stream(1,2,3,4)

Stream(1,2,3,4).toZipper

Stream(1,2,3,4).toZipper >>= {z:Zipper[Int] => z.next} >>= {z:Zipper[Int] => z.next}

Stream(1,2,3,4).toZipper >>= {z:Zipper[Int] => z.next} >>= {z:Zipper[Int] => z.previous}

val x1 = Stream(1,2,3,4).toZipper >>= {z:Zipper[Int] => z.next} >>= {z:Zipper[Int] => z.next} >>= {z:Zipper[Int] => z.modify {x: Int => 7 + x}.some}
x1.get.toStream.toList

//You can also use for comprehension syntax on Zippers
for {
  z <- Stream(1,2,3,4).toZipper
  n1 <- z.next
  n2 <- n1.next
} yield (n2.modify {x: Int => x + 12})
