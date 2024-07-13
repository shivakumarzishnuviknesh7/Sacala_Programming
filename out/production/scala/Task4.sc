import scala.annotation.tailrec

def isPrime(n: Int): Boolean = {
    def loop(n: Int, acc: Int): Boolean = n match {
      case n if n == acc => true
      case n if n % acc == 0 => false
      case _ => loop(n, acc + 1)
    }
    if (n < 2) false
    else if (n == 2) true
    else loop (n, 2)
  }

  def findSmallestPrime(a: Int, b: Int): Option[Int] = {
    @tailrec
    def loop(a: Int, b: Int, i: Int): Option[Int] = i match {
      case i if i > b => None
      case i if isPrime(i) => Some(i)
      case _ => loop(a, b, i + 1)
    }
    loop(a, b, a)
  }

  findSmallestPrime(0, 3) == Some(2)
  findSmallestPrime(-2, 1) == None
  findSmallestPrime(5, 7) == Some(5)
  findSmallestPrime(20, 22) == None
  findSmallestPrime(4, 5) == None

  def sumPrimes(a: Int, b: Int, c: Int):Option[Int] = {
    @tailrec
    def loop(a: Int, b: Int, i: Int): Option[Int] = i match {
      case i if i > b => None
      case i if isPrime(i) => Some(i)

      case _ => loop(a, b, i + 1)
    }
    loop(a, b, a)
  }

  sumPrimes(2, 4, 6) == Some(7)
  sumPrimes(3, 8, 10) == None
  sumPrimes(6, 7, 8) == None
