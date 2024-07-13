// Task 1
def max(a: Int, b: Int) = if (a > b) a else b
def evaluate(a: Int, b: Int, c: Int, f: (Int, Int) => Int) =
  f(f(a, b), c)
evaluate(1, 2, 3, max) == 3
evaluate(2, 1, 3, max) == 3
evaluate(3, 1, 2, max) == 3
evaluate(1, 3, 2, max) == 3
evaluate(2, 3, 1, max) == 3
evaluate(3, 2, 1, max) == 3

def min(a: Int, b: Int) = if (a < b) a else b

evaluate(1, 2, 3, min) == 1
evaluate(3, 5, 2, min) == 2

// Task 2
def findFirst(a: Int, b: Int, f: Int => Boolean) = {
  def loop(a: Int, b: Int, f: Int => Boolean): Int = a match {
    case a if a > b => -1
    case a if f(a) => a
    case _ => loop(a + 1, b, f)
  }

  loop(a, b, f)
}
findFirst(0, 1, x => x % 2 == 0) == 0
findFirst(1, 3, x => x % 2 == 0) == 2
findFirst(2, 4, x => x % 5 == 0) == -1
findFirst(2, 5, x => x % 5 == 0) == 5
findFirst(15, 20, x => x % 5 == 0) == 15

// Task 3
def sumUp(a: Int, b: Int, f: Int => Boolean) = {
  def loop(a: Int, b: Int, acc: Int, f: Int => Boolean): Int = a match {
    case a if a > b => acc
    case a if f(a) => loop(a + 1, b, a + acc, f)
    case _ => loop(a + 1, b, acc, f)
  }

  loop(a, b, 0, f)
}

sumUp(0, 1, _ % 2 == 0) == 0
sumUp(0, 2, _ % 2 == 0) == 2
sumUp(0, 3, _ % 2 == 0) == 2
sumUp(0, 4, _ % 2 == 0) == 6
sumUp(2, 3, _ % 2 == 0) == 2
sumUp(2, 6, _ % 2 == 0) == 12
sumUp(0, -1, _ % 2 == 0) == 0
sumUp(0, 3, x => x * x < 10) == 6
sumUp(0, 16, x => x * x < 10) == 6

// Task 4
def forAll(a: Int, b: Int, f: Int => Boolean): Boolean = a match {
  case a if (a > b) => true
  case a if f(a) => forAll(a + 1, b, f)
  case _ => false
}

forAll(1, 3, x => x < 5)
forAll(1, 3, x => x < 3)

// Task 5
def foldRight(a: Int, b: Int, unit: Int, f: Int => Boolean, g: (Int, Int) => Int): Int = {
  def loop(a: Int, b: Int, acc: Int, f: Int => Boolean, g: (Int, Int) => Int): Int = a match {
    case a if a > b => acc
    case a if f(a) => loop(a + 1, b, g(acc, a), f, g)
    case _ => loop(a + 1, b, acc, f, g)
  }

  loop(a, b, unit, f, g)
}

foldRight(1, 4, 0, x => true, _ + _) == 10
foldRight(1, 4, 1, _ => true, _ * _) == 24
foldRight(1, 4, 0, _ % 2 == 0, _ + _) == 6
foldRight(1, 7, 1, _ % 3 == 0, _ * _) == 18

// Task 6
def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

compose[Int, Int, Int](_ - 2, _ + 2)(4) == 4

// Task 7
def filter(s: String, f: Char => Boolean): String = {
  def loop(s: String, acc: String, f: Char => Boolean): String = s match {
    case s if s.isEmpty => acc
    case s if f(s.head) => loop(s.tail, s"$acc${s.head.toString}", f)
    case _ => loop(s.tail, acc, f)
  }

  loop(s, "", f)
}

filter("AaSsDd123wW", _.isUpper) == "ASDW"
filter("AaSsDd123wW", _.isLower) == "asdw"
filter("AaSsDd123wW", _.isLetter) == "AaSsDdwW"
filter("AaS1s1Dd123wW", _.isDigit) == "11123"

// Task 8
def map(s: String, f: Char => Char): String = {
  def loop(s: String, acc: String, f: Char => Char): String = s match {
    case s if s.isEmpty => acc
    case _ => loop(s.tail, s"${acc}${f(s.head)}", f)
  }

  loop(s, "", f)
}

map("AaS1s1Dd123wW", _.toUpper) == "AAS1S1DD123WW"
map("AaS1s1Dd123wW", _.toLower) == "aas1s1dd123ww"

def f(c: Char): Char = c match {
  case c if c.isLetter => c
  case _ => ' '
}

map("AaS1s1Dd123wW", f) == "AaS s Dd   wW"
