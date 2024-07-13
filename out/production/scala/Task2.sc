def checkFilter(a: Int, b: Int, f: Int => Boolean): Boolean = {
  def loop(a: Int, b: Int, f: Int => Boolean, acc: Int, i: Int): Boolean = i match {
    case i if acc >= 2 => true
    case i if i > b && acc < 2 => false
    case i if f(i) => loop(a, b, f, acc + 1, i+1)
    case _ => loop(a, b, f, acc, i+1)
  }
  loop(a, b, f, 0, a)
}

  def f1(n: Int) = n > 2
  checkFilter(1, 4, f1) == true
  checkFilter(2, 3, f1) == false
  checkFilter(3, 2, f1) == false
  checkFilter(4, 9, f1) == true

  def mod5(n: Int) = n % 5 == 0
  checkFilter(1, 10, mod5) == true
  checkFilter(1, 15, mod5) == true
  checkFilter(1, 9, mod5) == false
  checkFilter(6, 5, mod5) == false
