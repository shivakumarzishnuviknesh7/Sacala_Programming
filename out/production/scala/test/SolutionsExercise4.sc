// Task 1: count down from n to 0

def countDown(n: Int) = {
  def loop (n: Int, acc: String): String = n match {
    case n if n < 0 => acc
    case _ => loop(n - 1, s"$acc$n ")
  }
  loop(n, "").trim
}
countDown(-1) == ""
countDown(0) == "0"
countDown(1) == "1 0"
countDown(5) == "5 4 3 2 1 0"

// Task 2: check whether a string is a palindrome

def checkPalindrome(s: String): Boolean = s match {
  case "" => true
  case s if s.length == 1 => true
  case _ if s.head == s.last => checkPalindrome(s.substring(1, s.length - 1))
  case _ => false
}
checkPalindrome("") == true
checkPalindrome("a") == true
checkPalindrome("ab") == false
checkPalindrome("aa") == true
checkPalindrome("aba") == true
checkPalindrome("abba") == true
checkPalindrome("abca") == false

// Task 3: sum the digits of a number

def sumOfDigits(n: Int) = {
  def loop(n: Int, acc: Int): Int = n match {
    case 0 => acc
    case n if n < 0 => loop(-n, acc)
    case _ => loop(n / 10, acc + n % 10)
  }
  loop(n, 0)
}

sumOfDigits(0) == 0
sumOfDigits(9) == 9
sumOfDigits(10) == 1
sumOfDigits(12) == 3
sumOfDigits(1024) == 7
sumOfDigits(654321) == 21
sumOfDigits(-1) == 1
sumOfDigits(-15) == 6

// Task 4: reverse digits of number 143 => 431

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
reverseDigits(0) == "0"
reverseDigits(1) == "1"
reverseDigits(9) == "9"
reverseDigits(10) == "01"
reverseDigits(341) == "143"
reverseDigits(-341) == "-143"


// Task 5: count a char in a string

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
countChar("a", 'b') == 0
countChar("ababaa", 'a') == 4

// Task 6: convert decimal number to binary

def decimalToBinary(n: Int ) = {
  def loop(n: Int, acc: String): String = n match {
    case 0 => acc
    case _  => loop(n / 2, "" + n % 2 + acc)
  }
  if (n == 0) "0"
  else if (n < 0) s"-${loop(-n, "")}"
  else loop(n, "")
}
decimalToBinary(0) == "0"
decimalToBinary(1) == "1"
decimalToBinary(2) == "10"
decimalToBinary(8) == "1000"
decimalToBinary(-1) == "-1"
decimalToBinary(-31) == "-11111"

// Task 7: create string with all substrings of given length of s separated by space

def allSubstringsWithLength(s: String, n: Int) = {
  def loop(s: String, n: Int, acc: String, index: Int): String = index match {
    case index if n <= 0 => ""
    case index if index + n > s.length => acc
    case _ => loop(s, n, s"$acc ${s.substring(index, index + n)}", index + 1)
  }
  loop(s, n, "", 0).trim
}

allSubstringsWithLength("",1) == ""
allSubstringsWithLength("a", 1) == "a"
allSubstringsWithLength("ab", 1) == "a b"
allSubstringsWithLength("ab", 2) == "ab"
allSubstringsWithLength("abcd", 2) == "ab bc cd"
allSubstringsWithLength("abcd", 3) == "abc bcd"
allSubstringsWithLength("abcd", 4) == "abcd"
allSubstringsWithLength("abcd", 0) == ""


// create string with all non empty substrings of s separated by space

def allSubstrings(s: String) = {
  def loop(s: String, acc:String, i: Int): String = i match {
    case i if i > s.length => acc
    case _ => loop(s, s"$acc ${allSubstringsWithLength(s, i)}", i + 1)
  }
  loop(s, "", 1).trim
}
allSubstrings("") == ""
allSubstrings("a") == "a"
allSubstrings("ab") == "a b ab"
allSubstrings("abcd") == "a b c d ab bc cd abc bcd abcd"