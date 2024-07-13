
// Task 1
def dayOfWeekToNumber(day: String) = day match {
  case "Monday" => Some(1)
  case "Tuesday" => Some(2)
  case "Wednesday" => Some(3)
  case "Thursday" => Some(4)
  case "Friday" => Some(5)
  case "Saturday" => Some(6)
  case "Sunday" => Some(7)
  case other => None
}
dayOfWeekToNumber("Monday") == Some(1)
dayOfWeekToNumber("Sunday") == Some(7)
dayOfWeekToNumber("asdf") == None

def dayOfWeekToNumber1(day: String) = day match {
  case "Monday" => Right(1)
  case "Tuesday" => Right(2)
  case "Wednesday" => Right(3)
  case "Thursday" => Right(4)
  case "Friday" => Right(5)
  case "Saturday" => Right(6)
  case "Sunday" => Right(7)
  case other => Left("invalid string")
}
dayOfWeekToNumber1("Monday") == Right(1)
dayOfWeekToNumber1("Sunday") == Right(7)
dayOfWeekToNumber1("asdf") == Left("invalid string")

// Task 2
def numberToMonth(n: Int) = n match {
  case 1 => Some("January", "Jan.")
  case 2 => Some("February", "Feb.")
  case 3 => Some("March", "Mar.")
  case 4 => Some("April", "Apr.")
  case 5 => Some("May", "May")
  case 6 => Some("June", "Jun.")
  case 7 => Some("July", "Jul.")
  case 8 => Some("August", "Aug.")
  case 9 => Some("September", "Sept.")
  case 10 => Some("October", "Oct.")
  case 11 => Some("November", "Nov.")
  case 12 => Some("December", "Dec.")
  case _ => None
}
numberToMonth(1) == Some(("January","Jan."))
numberToMonth(3) == Some(("March","Mar."))
numberToMonth(12) == Some(("December","Dec."))
numberToMonth(0) == None

def numberToMonth1(n: Int) = n match {
  case 1 => Right("January", "Jan.")
  case 2 => Right("February", "Feb.")
  case 3 => Right("March", "Mar.")
  case 4 => Right("April", "Apr.")
  case 5 => Right("May", "May")
  case 6 => Right("June", "Jun.")
  case 7 => Right("July", "Jul.")
  case 8 => Right("August", "Aug.")
  case 9 => Right("September", "Sept.")
  case 10 => Right("October", "Oct.")
  case 11 => Right("November", "Nov.")
  case 12 => Right("December", "Dec.")
  case _ => Left("invalid number")
}
numberToMonth1(1) == Right(("January","Jan."))
numberToMonth1(3) == Right(("March","Mar."))
numberToMonth1(12) == Right(("December","Dec."))
numberToMonth1(0) == Left("invalid number")

// Task 3
def mean(xs: List[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
def variance(xs: List[Double]): Option[Double] =
  for {
    m <- mean(xs)
    zs <- Some(xs.map(y => (y - m)*(y - m)))
  } yield zs.sum
variance(List(1, 2, 3)) == Some(2.0)
variance(List()) == None
variance(List(1)) == Some(0.0)
variance(List(0, 1)) == Some(0.5)

// Task 4
def prime(n: Int): Boolean = {
  def loop(n: Int, acc: Int): Boolean = n match {
    case n if n == acc => true
    case n if n % acc == 0 => false
    case _ => loop(n, acc + 1)
  }

  if (n < 2) false
  else if (n == 2) true
  else loop(n, 2)
}
def smallestPrime(a: Int, b: Int): Option[Int] = a match {
  case a if a >= b => None
  case a if prime(a) => Some(a)
  case _ => smallestPrime(a + 1, b)
}
smallestPrime(2, 2) == None
smallestPrime(2, 3) == Some(2)
smallestPrime(4, 16) == Some(5)
smallestPrime(14, 16) == None
smallestPrime(40, 50) == Some(41)

// Task 5
def primesList(xs: List[Int]): List[Option[Int]] = {
  def loop(xs: List[Int], acc: List[Option[Int]]): List[Option[Int]] = xs match {
    case Nil => acc
    case y :: Nil => acc
    case y :: ys => loop(ys, smallestPrime(y, ys.head) :: acc)
  }

  xs.length match {
    case 0 => List(None)
    case 1 => List(None)
    case _ => loop(xs, Nil)
  }
}
primesList(List()) == List(None)
primesList(List(3)) == List(None)
primesList(List(4, 6)) == List(Some(5))
primesList(List(4, 6, 10)) == List(Some(7), Some(5))
primesList(List(4, 14, 16)) == List(None, Some(5))
def primesSum(xs: List[Int]): Option[Int] = {
  val ys = primesList(xs)
  if (ys.contains(None)) None
  else Some(ys.flatten.sum)
}
primesSum(List()) == None
primesSum(List(3)) == None
primesSum(List(4, 6)) == Some(5)
primesSum(List(4, 6, 10)) == Some(12)
primesSum(List(4, 14, 16)) == None
primesSum(List(16, 20, 25, 30)) == Some(69)

// Task 6
def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  for{
    aa <- a
    bb <- b
  } yield f(aa, bb)
map2(Some(5), Some(7))((a, b) => a + b)
map2[Int, Int, Int](Some(5), None)((a, b) => a + b)
map2(Some("abc"), Some("def"))((a, b) => a + b)

// Task 7
def sequence[A](a: List[Option[A]]): Option[List[A]] =
a.foldRight(Option(List[A]()))((x, xs) => map2(x, xs)(_ :: _))
sequence(List(Some(2), None, Some(5))) == None
sequence(List(Some(2), Some(5))) == Some(List(2, 5))
sequence(List()) == Some(List())

// Task 8
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  sequence(a map f)
traverse(List(1, 2))(x => Some(2 * x)) == Some(List(2, 4))
traverse(List(1, 2))(x => if (x >= 2) Some(x) else None) == None
