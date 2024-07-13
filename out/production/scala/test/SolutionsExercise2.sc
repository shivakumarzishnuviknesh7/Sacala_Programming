// Task 1
def dayOfWeekToNumber(day: String) = day match {
  case "Monday" => 1
  case "Tuesday" => 2
  case "Wednesday" => 3
  case "Thursday" => 4
  case "Friday" => 5
  case "Saturday" => 6
  case "Sunday" => 7
  case other => -1
}
dayOfWeekToNumber("Monday")
dayOfWeekToNumber("Sunday")
dayOfWeekToNumber("asdf")

// Task 2
def numberToDayOfWeek(n: Int) = n match {
  case 1 => "Monday"
  case 2 => "Tuesday"
  case 3 => "Wednesday"
  case 4 => "Thursday"
  case 5 => "Friday"
  case 6 => "Saturday"
  case 7 => "Sunday"
  case _ => "not a day of week"
}
numberToDayOfWeek(1)
numberToDayOfWeek(5)
numberToDayOfWeek(7)
numberToDayOfWeek(8)

// Task 3
def monthToNumber(month: String) = month match {
  case "January" => 1
  case "February" => 2
  case "March" => 3
  case "April" => 4
  case "May" => 5
  case "June" => 6
  case "July" => 7
  case "August" => 8
  case "September" => 9
  case "October" => 10
  case "November" => 11
  case "December" => 12
  case _ => -1
}
monthToNumber("January")
monthToNumber("December")
monthToNumber("asdf")

// Task 4
def numberToMonth(n: Int) = n match {
  case 1 => ("January", "Jan.")
  case 2 => ("February", "Feb.")
  case 3 => ("March", "Mar.")
  case 4 => ("April", "Apr.")
  case 5 => ("May", "May")
  case 6 => ("June", "Jun.")
  case 7 => ("July", "Jul.")
  case 8 => ("August", "Aug.")
  case 9 => ("September", "Sept.")
  case 10 => ("October", "Oct.")
  case 11 => ("November", "Nov.")
  case 12 => ("December", "Dec.")
  case _ => ("not a month", "not a month")
}

numberToMonth(1)
numberToMonth(3)
numberToMonth(12)
numberToMonth(0)

// Task 5
def millisecondsToDays(n: Long) = {
  val fraction = n % 1000
  val allseconds = n / 1000
  val seconds = allseconds % 60
  val allminutes = allseconds / 60
  val minutes = allminutes % 60
  val allhours = allminutes / 60
  val hours = allhours % 24
  val days = allhours / 24
  s"$days days, $hours hours, $minutes minutes, $seconds.$fraction seconds"
}
millisecondsToDays(98423756)

// Task 6
def not(a: Boolean) = a match {
  case true => false
  case false => true
}
not(true)

def and(a: Boolean, b: Boolean) = (a, b) match {
  case (true, true) => true
  case _ => false
}

and(true, false)
and(true, true)

def or(a: Boolean, b: Boolean) = (a, b) match {
  case (false, false) => false
  case _ => true
}
or(false, false)
or(false, true)
or(true, false)
or(true, true)

def xor(a: Boolean, b: Boolean) = or(and(a, not(b)), and(not(a), b))
xor(false, false)
xor(false, true)
xor(true, false)
xor(true, true)

def implies(a: Boolean, b: Boolean) = or(not(a), b)
implies(false, false)
implies(false, true)
implies(true, false)
implies(true, true)

// Task 7
def min(a: Int, b: Int) = {
  if (a < b) a else b
}
def max(a: Int, b: Int) = {
  if (a > b) a else b
}
def minmax(a: Int, b: Int, c: Int, d: Int) = {
  val minNumber = min(a, min(b, min(c, d)))
  val maxNumber = max(a, max(b, max(c, d)))
  (minNumber, maxNumber)
}
minmax(1, 2, 3, 4)
minmax(2, 1, 4, 3)
minmax(4, 3, 2, 1)

// Task 8
def firstChars(a: String, b: String, c: String, d: String) = {
  val a1 = if (a.isEmpty) "" else a.charAt(0).toString
  val b1 = if (b.isEmpty) "" else b.charAt(0).toString
  val c1 = if (c.isEmpty) "" else c.charAt(0).toString
  val d1 = if (d.isEmpty) "" else d.charAt(0).toString
  s"$a1$b1$c1$d1"
}
firstChars("ab", "cd", "", "ed")
firstChars("The", "quick", "brown", "fox")
