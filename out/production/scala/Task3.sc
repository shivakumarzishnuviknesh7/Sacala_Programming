
def f(a: List[String]): String = a.foldLeft("")(g).reverse
  def g(a: String, b: String) = b match {
    case "" => a
    case _ => b.charAt(b.length() - 1).toString() + a
  }

  f(List("ca","db","ec","fd")) == "abcd"
  f(List("1a","","c","3d")) == "acd"
  f(List("","gggb","hhc","ddddd")) == "bcd"
  f(List("na","mb","gc","")) == "abc"
  f(List("2a","","","333d")) == "ad"
  f(List()) == ""
