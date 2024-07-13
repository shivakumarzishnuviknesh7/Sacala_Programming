val x = 12
val c1 : Int = (x * 9 )/5 +32
print(c1)
val c1 : Int = (1 + 2) * 3

val x1 = 12
if( x1 > 10 ) 12 else 14
if( x1 < 10 ) 12 else 14

val a = {}

"helfen" * 6

"ich komme".take(3)
"ich komme".drop(3)
"ich komme".takeRight(3)
"ich komme".dropRight(3)

val s1 = "kannen"
val s2 = "Sie"
print(s1<s2 )

val s3 = "du bist meine bruder"
s3.reverse

val y1 = 2
y1 match{
  case 1 => "gut"
  case 2 => "gross"
  case 3 => "mutter"
  case 4 => "Neuer"
}

def methodName(tag: String) = tag match {
  case "eins" => 1
  case "zwei" => 2
  case "dri" => 3
  case "vier" => 4
  case "funf" => 5
  case other => (-1,0)
}
methodName("zwei")
methodName("ich  bin brillant")

def millisecondsToDays(n: Int) = {
  val fraction = n % 1000
  val allseconds = n/1000
  val seconds = allseconds % 60
  val allminutes = allseconds / 60
  val minutes = allminutes % 60
  val allhours = allminutes / 60
  val hours = allhours % 24
  val days = allhours / 24

  s"$days days, $hours"
}
millisecondsToDays(98542375)

def not(a: Boolean) = a match {
  case true => false
  case false => true
}
not(true)

def normal(a: Boolean) = a match {
  case true => true
  case false => false
}
normal(true)


def and(a: Boolean, b: Boolean ) = (a,b)match{
  case(true,true) => true
  case _ => false

}
and(true,true)
and(true,false)

def or(a: Boolean, b: Boolean ) = (a,b)match{
  case(false,false) => false
  case _ => true

}
or(true,true)
or(false,false)




