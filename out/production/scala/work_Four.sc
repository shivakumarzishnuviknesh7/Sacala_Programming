def countDown(n: Int) = {
  def loop (n: Int, acc: String): String = n match {
    case n if n < 0 => acc
    case _ => loop(n - 1, s"$acc$n ")
  }
  loop(n, "").trim
}

countDown(0) == "0"
countDown(1) == "1 0"
countDown(5) == "5 4 3 2 1 0"
countDown(11) == "5 4 3 2 1 0"


def checkPalindrome(s: String): Boolean = s match {
  case "" => true
  case s if s.length == 1 => true
  case _ if s.head == s.last => checkPalindrome(s.substring(1, s.length - 1))
  case _ => false
}
checkPalindrome("") == true
checkPalindrome("ich kann") == true
checkPalindrome("wir sind") == false
checkPalindrome("malayalam") == true


def sumOfDigits(n: Int) = {
  def loop(n: Int, acc: Int): Int = n match {
    case 0 => acc
    case n if n < 0 => loop(-n, acc)
    case _ => loop(n / 10, acc + n % 10)
  }
  loop(n, 0)
}

sumOfDigits(11) == 12
sumOfDigits(1024) == 7
sumOfDigits(11) == 2


def reverseDigits(n: Int) = {
  def loop(n: Int, acc: String): String = n match {
    case n if n < 10 => acc + n
    case _ => loop(n / 10, acc + n % 10)
  }
  n match {
    case 0 => "0"
    case n if n < 0 => s"-${loop(-n, "")}"
    case _ => loop(n, "")
  }
}

reverseDigits(10) == "01"
reverseDigits(341) == "143"



def countChar(s: String, c: Char) = {
  def loop(s: String, c: Char, acc: Int): Int = s match {
    case "" => acc
    case s if s.charAt(0) == c => loop(s.substring(1), c, acc + 1)
    case _ => loop(s.substring(1), c, acc)
  }
  loop(s, c, 0)
}
countChar("", 'a') == 0
countChar("a", 'a') == 1
countChar("abaacd", 'a') == 2
countChar("abaacd", 'a') == 3



def decimalToBinary(n: Int ) = {
  def loop(n: Int, acc: String): String = n match {
    case 0 => acc
    case _  => loop(n / 2, "" + n % 2 + acc)
  }
  if (n == 0) "0"
  else if (n < 0) s"-${loop(-n, "")}"
  else loop(n, "")
}
decimalToBinary(3) == "0"
decimalToBinary(26) == "11010"
decimalToBinary(-1) == "-1"
decimalToBinary(-31) == "-11111"



def allSubstringsWithLength(s: String, n: Int) = {
  def loop(s: String, n: Int, acc: String, index: Int): String = index match {
    case index if n <= 0 => ""
    case index if index + n > s.length => acc
    case _ => loop(s, n, s"$acc ${s.substring(index, index + n)}", index + 1)
  }
  loop(s, n, "", 0).trim
}


allSubstringsWithLength("abcd", 2) == "ab bc cd"
allSubstringsWithLength("abcd", 3) == "abc bcd"
allSubstringsWithLength("abcd", 4) == "abcd"
allSubstringsWithLength("abcd", 0) == ""
