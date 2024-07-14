// Task 1
def removeFirst[A](xs: List[A], p: A => Boolean) = {
  def loop(xs: List[A], acc: List[A], p: A => Boolean): List[A] = xs match {
    case Nil => acc.reverse
    case xs if p(xs.head) => acc.reverse:::xs.tail
    case _ => loop(xs.tail, xs.head::acc,  p)
  }
  loop(xs, Nil, p)
}
removeFirst(List[Int](), _ % 2 == 0) == Nil
removeFirst(List(1, 2, 3, 4), _ == 1) == List(2, 3, 4)
removeFirst(List(1, 2, 3, 4), _ % 2 == 0) == List(1, 3, 4)
removeFirst(List(1, 2, 3, 4), _ == 4) == List(1, 2, 3)
removeFirst(List(1, 2, 3, 4), _ == 5) == List(1, 2, 3, 4)
removeFirst(List("ab", "cd", "ed"), _ == "cd") == List("ab", "ed")

def removeFirst1[A](xs: List[A], p: A => Boolean): List[A] = {
  val (ys, zs) = (xs.takeWhile(!p(_)), xs.dropWhile(!p(_)))
  if (zs.isEmpty) ys else ys:::zs.tail
}
removeFirst1(List[Int](), _ % 2 == 0) == Nil
removeFirst1(List(1, 2, 3, 4), _ == 1) == List(2, 3, 4)
removeFirst1(List(1, 2, 3, 4), _ % 2 == 0) == List(1, 3, 4)
removeFirst1(List(1, 2, 3, 4), _ == 4) == List(1, 2, 3)
removeFirst1(List(1, 2, 3, 4), _ == 5) == List(1, 2, 3, 4)
removeFirst1(List("ab", "cd", "ed"), _ == "cd") == List("ab", "ed")

// Task 2
def removeDuplicates[A](xs: List[A]): List[A] = {
  def loop[A](xs: List[A], acc: List[A]): List[A] = xs match {
    case Nil => acc
    case xs if xs.tail == Nil => xs.head :: acc
    case xs if xs.head == xs.tail.head => loop(xs.tail, acc)
    case _ => loop(xs.tail, xs.head :: acc)
  }
  loop(xs, Nil).reverse
}
removeDuplicates(List(1, 1)) == List(1)
removeDuplicates(List(1, 1, 1, 3, 3, 5)) == List(1, 3, 5)
removeDuplicates(List("ab", "ab", "def", "gh", "gh", "kl")) == List("ab", "def", "gh", "kl")
removeDuplicates(List("sdf")) == List("sdf")
removeDuplicates(Nil) == Nil

def removeDuplicates1[A](xs: List[A]): List[A] =
  xs.foldLeft[List[A]](Nil)((as: List[A], a: A) => if (as.isEmpty ||as.head != a) a::as else as).reverse
removeDuplicates1(List(1, 1)) == List(1)
removeDuplicates1(List(1, 1, 1, 3, 3, 5)) == List(1, 3, 5)
removeDuplicates1(List("ab", "ab", "def", "gh", "gh", "kl")) == List("ab", "def", "gh", "kl")
removeDuplicates1(List("sdf")) == List("sdf")
removeDuplicates1(Nil) == Nil

//Task 3
def pack[A](xs: List[A]) = {
  def loop(xs: List[A], acc: List[List[A]]): List[List[A]] = xs match {
    case Nil => acc
    case xs if acc.isEmpty => loop(xs.tail, List(List(xs.head)))
    case xs if xs.head == acc.head.head => loop(xs.tail, (xs.head::acc.head)::acc.tail)
    case _ => loop(xs.tail, (xs.head::Nil)::acc)
  }
  loop(xs, Nil).reverse
}
pack(Nil) == List()
pack(List(1, 2, 3)) == List(List(1), List(2), List(3))
pack(List(1, 1, 1, 2, 3, 3, 4, 5, 5, 5, 5)) == List(List(1, 1, 1), List(2), List(3, 3), List(4), List(5, 5, 5, 5))
pack(List("ab", "cde", "cde", "fg")) == List(List("ab"),  List("cde", "cde"), List("fg"))

def pack1[A](xs: List[A]) = {
  def loop(xs: List[A], acc: List[List[A]]): List[List[A]] = xs match {
    case Nil => acc
    case _ => loop(xs.dropWhile(_ == xs.head), xs.takeWhile(_ == xs.head)::acc )
  }
  loop(xs, Nil).reverse
}
pack1(Nil) == List()
pack1(List(1, 2, 3)) == List(List(1), List(2), List(3))
pack1(List(1, 1, 1, 2, 3, 3, 4, 5, 5, 5, 5)) == List(List(1, 1, 1), List(2), List(3, 3), List(4), List(5, 5, 5, 5))
pack1(List("ab", "cde", "cde", "fg")) == List(List("ab"),  List("cde", "cde"), List("fg"))

// Task 4
def encode[A](xs: List[A]) = {
  val as = pack(xs)
  def loop(as: List[List[A]], acc: List[(A, Int)]): List[(A, Int)] = as match{
    case Nil => acc
    case _ => loop(as. tail, (as.head.head, as.head.length):: acc)
  }
  loop(as, Nil).reverse
}
encode(Nil) == List()
encode(List(1, 2, 3)) == List((1, 1), (2, 1), (3, 1))
encode(List(1, 1, 1, 2, 3, 3, 4, 5, 5, 5, 5)) == List((1, 3), (2, 1), (3, 2), (4, 1), (5, 4))
encode(List("ab", "cde", "cde", "fg")) == List(("ab", 1),  ("cde", 2), ("fg", 1))

def encode1[A](xs: List[A]) = pack1(xs).map(x => (x.head, x.length))
encode1(Nil) == List()
encode1(List(1, 2, 3)) == List((1, 1), (2, 1), (3, 1))
encode1(List(1, 1, 1, 2, 3, 3, 4, 5, 5, 5, 5)) == List((1, 3), (2, 1), (3, 2), (4, 1), (5, 4))
encode1(List("ab", "cde", "cde", "fg")) == List(("ab", 1),  ("cde", 2), ("fg", 1))

