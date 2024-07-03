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














