def factorial(n: Int): Int = {
  def loop(n: Int,  acc: Int): Int = n match {
    case n if n < 0 => -1
    case 0 => acc
    case _ => loop(n - 1, acc * n)
  }
  loop(n, 1)
}

factorial(3) == 3
factorial(0) == 1
factorial(1) == 1


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

prime(-2)
prime(1)
prime(2)
prime(3)

def countPrimeNumbers(n: Int): Int = {
  def loop(n: Int, acc: Int): Int = n match {
    case n if n <= 1 => acc
    case n if prime(n) => loop(n - 1, acc + 1)
    case _ => loop(n - 1, acc)
  }

  loop(n, 0)
}

countPrimeNumbers(1)
countPrimeNumbers(2)
countPrimeNumbers(3)


def findPrimNumberDecomposition(n: Int): (Int, Int) = {
  def loop(n: Int, acc: Int): (Int, Int) =
    if (prime(acc) && prime(n - acc)) (acc, n - acc)
    else loop(n, acc + 1)

  if (n < 4 || n % 2 != 0) (0, 0)
  else loop(n, 2)
}

findPrimNumberDecomposition(1)
findPrimNumberDecomposition(4)
findPrimNumberDecomposition(6)


def gcd(a: Int, b: Int): Int =
  if (b == 0)
    a
  else
    gcd(b, a % b)

gcd(2, 2)
gcd(2, 3)


def powerOf2(n: Int): Boolean = n match {
  case n if n < 1 => false
  case n if n == 1 => true
  case n if n % 2 != 0 => false
  case _ => powerOf2(n / 2)
}

powerOf2(0)
powerOf2(1)
powerOf2(2)
powerOf2(3)

def numberOfDivisors(n: Int): Int = {
  def isDivisor(n: Int, a: Int) = n % a == 0
  def loop(n: Int, a: Int, acc: Int): Int =
    if (a == n) acc + 1
    else {
      val b = if (isDivisor(n, a)) acc + 1 else acc
      loop(n, a + 1, b)
    }

  loop(n, 1, 0)
}

numberOfDivisors(1)
numberOfDivisors(2)
numberOfDivisors(3)
numberOfDivisors(4)
numberOfDivisors(13)


def findSmallestNumber(): Int = {
  def loop(acc: Int): Int =
    if (!powerOf2(gcd(numberOfDivisors(acc), numberOfDivisors(acc * acc * acc)))) acc else loop(acc + 1)

  loop(1)
}
findSmallestNumber()
