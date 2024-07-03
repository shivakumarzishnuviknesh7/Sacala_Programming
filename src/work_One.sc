"hallo"
var i = 9;
var num: Int =7;
i + num
i * num
var name: String="trial"

var num1: Int =7;
num1 = 1
var num2: Int =7;
println(num1 + num2)

//val num1: Int =7;/* val constant or final */
num1 = 1
//var num2: Int =7;
println(num1 + num2)


var result =  8 .+ (7);

/* class */                              /* constructor */
case class Student(var rollno: Int=1 ,var name: String="data1" ,var marks: Int=90){
}
/* object creation */
var s1= Student();/* default */

/* class */                              /* constructor */
case class Student1(var rollno: Int=1 ,var name: String="data1" ,var marks: Int=90){
  /* function or method */
  def show()={
    println("hi")
  }
  def > (s2 : Student1) : Boolean = marks > s2.marks
}
/* object creation */
var s2= Student1(1,"data3",80);/* constructor overloading */
var s3= Student1(name="data2");
s2.show()

s3.>(s2)


//list
var numbers = List(1,2,3,4)

for(n <- numbers){
  println(n)
}

var number = List(1,2,3,4)
number.foreach { j => println(j) }

var reversevalue = number.reverse
number.drop(2)
number.take(1)

// x° Celsius (°C) to y° Fahrenheit (°F) is y = (x*9/5)+32
val input_temp = 12
val c1 : Double = input_temp * 9
val c2 : Double = c1 / 5
val c3 = c2 + 32
val d1: Int = input_temp * 9
val d2: Int = d1 / 5
val d3: Int = d2 + 32


val x1 = 12
if (x1 > 16) 12 else 14
if (x1 > 10) 12 else 14L

val d = {}

"good" * 3

"apple".take(3)
"apple".drop(2)
"apple".takeRight(3)
"apple".dropRight(3)


val s1 = "asdf"
val s2 = "qwer"
s1 < s2
s1 > s2

val s = "orange"
s + " " + s.reverse

val y1 = 4
y1 match {
  case 1  => "January"
  case 2  => "February"
  case 3  => "March"
  case 4  => "April"
  case 5  => "May"
  case 6  => "June"
  case 7  => "July"
  case 8  => "August"
  case 9  => "September"
  case 10 => "October"
  case 11 => "November"
  case 12 => "December"
}

val month = "April"
month match {
  case "January"   => 1
  case "February"  => 2
  case "March"     => 3
  case "April"     => 4
  case "May"       => 5
  case "June"      => 6
  case "July"      => 7
  case "August"    => 8
  case "September" => 9
  case "October"   => 10
  case "November"  => 11
  case "December"  => 12
}

val s3 = "Monday"
s3 match {
  case "Monday"    => 1
  case "MON"       => 1
  case "Tuesday"   => 2
  case "TUE"       => 2
  case "Wednesday" => 3
  case "WED"       => 3
  case "Thursday"  => 4
  case "THU"       => 4
  case "Friday"    => 5
  case "FRI"       => 5
  case "Saturday"  => 6
  case "SAT"       => 6
  case "Sunday"    => 7
  case "SUN"       => 7
}













