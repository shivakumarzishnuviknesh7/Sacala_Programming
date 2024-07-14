// Task 1
trait RNG { def nextInt: (Int, RNG) }
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFl
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, r1) = rng.nextInt
  val n1 = if (n < 0) - n - 1 else n
  (n1, r1)
}
val r1 = SimpleRNG(100)
val (n2, r2 ) = nonNegativeInt(r1)
val (n3, r3 ) = nonNegativeInt(r2)
val (n4, r4 ) = nonNegativeInt(r3)
val (n5, r5 ) = nonNegativeInt(r4)
// Task 2
def double(rng: RNG): (Double, RNG) = {
  val (n, r1) = nonNegativeInt(rng)
  (n / (Int.MaxValue.toDouble + 1), r1)
}
val (d1, rd1) = double(r1)
val (d2, rd2) = double(rd1)
val (d3, rd3) = double(rd2)
val (d4, rd4) = double(rd3)
val (d5, rd5) = double(rd4)
val (d6, rd6) = double(rd5)
// Task 3
def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (n, r1) = rng.nextInt
  val (d, r2) = double(r1)
  ((n, d), r2)
}
val ((i7,d7),r7) = intDouble(r1)
val ((i8,d8),r8) = intDouble(r7)
val ((i9,d9),r9) = intDouble(r8)
val ((i10,d10),r10) = intDouble(r9)
val ((i11,d11),r11) = intDouble(r10)
val ((i12,d12),r127) = intDouble(r11)
// Task 4
object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}

case class State[S, +A] (run: S => (A, S)){
  def flatMap[B](f: A => State[S, B]): State[S, B] = State( s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
}

case class Result(value: Int)
def eval(op: Char, a: Int): State[Result, Int] = State((r: Result) => op match {
  case '+' => (r.value + a, Result(r.value + a))
  case '-' => (r.value - a, Result(r.value - a))
  case '*' => (r.value - a, Result(r.value * a))
  case _ => (r.value, r)
})
val rs0: State[Result, Int] = for {
  _ <- eval('+', 2)
  _ <- eval('-', 5)
  _ <- eval('*', 4)
  a <- eval('x', 5)
} yield a
rs0.run(Result(0))._1 == -12
// Task 5
case class Result1(value: Option[Int])
def eval1(op: Char, a: Int): State[Result1, Option[Int]] = State((r: Result1) => (op, a) match {
  case ('+', a) => {val a1 = for {b <- r.value } yield b + a; (a1, Result1(a1))}
  case ('-', a) => {val a1 = for {b <- r.value } yield b - a; (a1, Result1(a1))}
  case ('*', a) => {val a1 = for {b <- r.value } yield b * a; (a1, Result1(a1))}
  case ('/', 0) => (None, Result1(None))
  case ('/', a) => {val a1 = for {b <- r.value } yield b / a; (a1, Result1(a1))}
  case _ => (None, Result1(None))
})
val rs1: State[Result1,Option[Int]] = for {
  _ <- eval1('+', 2)
  _ <- eval1('-', 5)
  _ <- eval1('*', 4)
  a <- eval1('/', 3)
} yield a
rs1.run(Result1(Some(0)))._1 == Some(-4)
val rs2: State[Result1,Option[Int]] = for {
  _ <- eval1('+', 2)
  _ <- eval1('/', 0)
  _ <- eval1('*', 4)
  a <- eval1('/', 3)
} yield a
rs2.run(Result1(Some(0)))._1 == None
val rs3: State[Result1,Option[Int]] = for {
  _ <- eval1('x', 2)
  a <- eval1('/', 3)
} yield a
rs3.run(Result1(Some(9)))._1 == None
