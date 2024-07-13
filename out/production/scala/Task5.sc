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

  case class StringState(value: String)
  def update(s: String) : State[StringState, String] = State((m: StringState) => {
    (m.value + " " + s, StringState(m.value + " " + s))
  })
  val s1: State[StringState, String] = for {
    _ <- update("a")
    _ <- update("bc")
    r <- update("def")
  } yield r
  s1.run(StringState("a"))
