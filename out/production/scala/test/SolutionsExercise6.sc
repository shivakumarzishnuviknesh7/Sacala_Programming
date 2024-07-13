// Task 1
def isFactor(n: Int, a: Int) = n match {
  case n if n <= 1 => false
  case n if a <= 0 => false
  case n if n == a => true
  case _ => n % a == 0
}
def factors(n: Int): List[Int] = {
  def loop(n: Int, a: Int, acc: List[Int]): List[Int] = n match {
    case n if a > n => acc
    case n if isFactor(n, a) => loop (n, a + 1, a:: acc)
    case _ => loop(n, a + 1, acc)
  }
  loop(n, 2, Nil)
}

factors(1) == List()
factors(2) == List(2)
factors(3) == List(3)
factors(4) == List(4, 2)
factors(8) == List(8, 4, 2)
factors(12) == List(12, 6, 4, 3, 2)
factors(24) == List(24, 12, 8, 6, 4, 3, 2)
factors(0) == List()
factors(-1) == List()

// Task 2
def longestString(xs: List[String]): String =
  xs.foldLeft("")((s1: String, s2: String) => if (s1.length >= s2.length) s1 else s2)
longestString(List("ab", "def")) == "def"
longestString(List("ab", "defg", "qw")) == "defg"
longestString(List()) == ""

// Task 3
def duplicate[A](xs: List[A], n: Int): List[A] = {
  xs flatMap(x => List.fill(n)(x))
}
duplicate(List(2, 4), 3) == List(2, 2, 2, 4, 4, 4)
duplicate(Nil, 4) == List()
duplicate(List("ab","def", "qwer"), 5) ==
  List("ab", "ab", "ab", "ab", "ab", "def", "def", "def", "def", "def", "qwer", "qwer", "qwer", "qwer", "qwer")

// Task 4
def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = {
  val (a, b ) = ls.splitAt(n)
  a ::: e :: b
}
insertAt(2, 0, List(3, 4, 5, 6)) == List(2, 3, 4, 5, 6)
insertAt(2, 1, List(3, 4, 5, 6)) == List(3, 2, 4, 5, 6)
insertAt(2, 2, List(3, 4, 5, 6)) == List(3, 4, 2, 5, 6)
insertAt(2, 3, List(3, 4, 5, 6)) == List(3, 4, 5, 2, 6)
insertAt(2, 4, List(3, 4, 5, 6)) == List(3, 4, 5, 6, 2)
insertAt(2, 1, Nil) == List(2)

// Task 5
def removeAll[A](ls: List[A], k: Int) =
  if (k < 1) ls
  else ls.zipWithIndex filter ( v => (v._2 + 1) % k != 0) map(_._1)

removeAll(List(1, 2, 3, 4, 5, 6, 7), 2) == List(1, 3, 5, 7)
removeAll(List(1, 2, 3, 4, 5, 6, 7), 3) == List(1, 2, 4, 5, 7)
removeAll(List(1, 2, 3, 4, 5, 6, 7), 1) == List()
removeAll(List(1, 2, 3, 4, 5, 6, 7), 0) == List(1, 2, 3, 4, 5, 6, 7)
removeAll(Nil, 2) == List()

// Task 6
def rotate[A](ls: List[A], k: Int): List[A] = (k, ls) match {
  case (k, _) if k < 1 => ls
  case (k, Nil) => ls
  case _ => (ls drop k % ls.length) ::: (ls take k % ls.length)
}
rotate(List(1, 2, 3, 4, 5), 1) == List(2, 3, 4, 5, 1)
rotate(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5, 1, 2)
rotate(List(1, 2, 3, 4, 5), 5) == List(1, 2, 3, 4, 5)
rotate(List(1, 2, 3, 4, 5), 6) == List(2, 3, 4, 5, 1)
rotate(List(1, 2, 3, 4, 5), 0) == List(1, 2, 3, 4, 5)
rotate(Nil, 2) == List()

// Task 7
def isSublist[A](xs: List[A], sublist: List[A]): Boolean = xs match {
  case xs if xs startsWith sublist => true
  case Nil => false
  case _ => isSublist(xs.tail, sublist)
}
isSublist(List(1, 2, 3), List(1)) == true
isSublist(List(1, 2, 3), List(2)) == true
isSublist(List(1, 2, 3), List(3)) == true
isSublist(List(1, 2, 3), List(4)) == false
isSublist(List(1, 2, 3), List(1, 2)) == true
isSublist(List(1, 2, 3), List(2, 3)) == true
isSublist(List(1, 2, 3), List(2, 3, 4)) == false
isSublist(Nil, List(1)) == false
