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

