// Task 1
abstract class BinaryTree
case class Leaf(value: Int) extends BinaryTree
case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

def leafSum(tree: BinaryTree): Int = tree match {
  case Leaf(i) => i
  case null => 0
  case Node(l, r) => leafSum(l) + leafSum(r)
}
leafSum(Leaf(2))
val btree = Node(
  Node(
    Node(
      Leaf(3),
      Leaf(8)
    ),
    Leaf(5)
  ),
  Leaf(2))
leafSum(btree)
val btree1 = Node(null, Leaf(3))
leafSum(btree1)

// Task 2
sealed abstract class Tree
case class Leaf1(value: Int) extends Tree
case class Node1(children: List[Tree]) extends Tree

def leafSum1(tree: Tree): Int = tree match {
  case Leaf1(i) => i
  case null => 0
  case Node1(children) => children.map(x => leafSum1(x)).sum
}
val tree = Leaf1(2)
leafSum1(tree)
val tree1 = Node1(List(null))
leafSum1(tree1)
val tree2 = Node1(List(tree,tree1))
leafSum1(tree2)
val tree3 = Node1(List(Leaf1(1), Leaf1(2), Leaf1(3)))
val tree4 = Node1(List(Leaf1(4), null, Leaf1(5)))
val tree5 = Node1(List(tree3, Leaf1(6), tree4))
leafSum1(tree5)
leafSum1(Node1(List()))

// Task 3
case class Node2(op: Char, left: BinaryTree, right: BinaryTree) extends BinaryTree

def eval(tree: BinaryTree): Option[Int] = tree match {
  case Leaf(i) => Some(i)
  case Node2('+', left, right) => for { a <- eval(left); b <- eval(right) } yield a + b
  case Node2('-', left, right) => for { a <- eval(left); b <- eval(right) } yield a - b
  case Node2('*', left, right) => for { a <- eval(left); b <- eval(right) } yield a * b
  case Node2('/', left, right) if eval(right) == Some(0) => None
  case Node2('/', left, right) => for { a <- eval(left); b <- eval(right) } yield a / b
  case _ => None
}
val tree6 = Leaf(2)
eval(tree6) == Some(2)
eval(null) == None
eval(Node2('+', tree6, null)) == None
val tree7 = Node2('+', Leaf(3), Leaf(5))
eval(tree7) == Some(8)
val tree8 = Node2('-', Leaf(4), Leaf(7))
eval(tree8) == Some(-3)
val tree9 = Node2('*', Leaf(3), Leaf(8))
eval(tree9) == Some(24)
val tree10 = Node2('/', Leaf(13), Leaf(3))
eval(tree10) == Some(4)
val tree11 = Node2('/', Leaf(7), Leaf(0))
eval(tree11) == None
val tree12 = Node2('+',tree7, tree8)
eval(tree12) == Some(5)
val tree13 = Node2('+',tree7, null)
eval(tree13) == None
val tree14 = Node2('/', tree8, Node2('-', Leaf(4), Leaf(4)))
eval(tree14) == None
val tree15 = Node2('a', Leaf(3), Leaf(6))
eval(tree15) == None
val tree16 = Node2('*', tree7, tree12)
eval(tree16) == Some(40)

// Task 4
trait Odered[A]{
  def compare(a: A): Int
  def <(a: A) = compare(a) < 0
  def <=(a: A) = compare(a) < 0 || compare(a) == 0
  def >(a: A) = ! <=(a)
  def >=(a: A) = ! <(a)
}

class Point2D(val x: Int, val y: Int)
class OrderedPoint2D(x: Int, y: Int) extends Point2D(x, y) with Ordered[Point2D]{
  override def compare(that: Point2D) = if (x != that.x) x - that.x else y - that.y
}
val p1 = new OrderedPoint2D(1,3)
val p2 = new OrderedPoint2D(1,4)
p1 < p2
p1 <= p2
val p3 = new OrderedPoint2D(0,3)
p1 < p3
p1 <= p3

class Point3D(val x: Int, val y: Int, val z: Int)
class OrderedPoint3D(x: Int, y: Int, z: Int) extends Point3D(x, y, z) with Ordered[Point3D]{
  override def compare(that: Point3D) =  if (x != that.x) x - that.x else if (y != that.y) y - that.y else z - that.z
}
val p4 = new OrderedPoint3D(1, 2, 3)
val p5 = new OrderedPoint3D(2, 2, 2)
val p6 = new OrderedPoint3D(1, 3, 3)
val p7 = new OrderedPoint3D(1, 2, 4)
p4 < p5
p4 < p5
p4 < p7
p4 <= p6
p5 >= p6
p6 < p7

