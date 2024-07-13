def isPolynom(n: Int): Boolean = {
  def loop(n: Int, i: Int): Boolean = n match {
    case n if i*i*i + i == n => true
    case n if (i*i*i + i > n) => false
    case _ => loop(n, i+1)
  }
  if (n < 0) loop(-n, 0)
  else loop(n,0)
}

  isPolynom(0)  == true
  isPolynom(1)  == false
  isPolynom(2)  == true
  isPolynom(10)  == true
  isPolynom(16) == false
  isPolynom(1000100) == true
  isPolynom(-2) == true
  isPolynom(-10) == true
  isPolynom(-16) == false
  