package Chapter6




case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B): State[S,B] = {
    val g: S => (B,S) = s => {
      val (a,ss) = run(s)
      (f(a), ss)
    }
    State(g)
  }

  def flatMap[B](f: A => State[S,B]): State[S,B] = {
    val g: S => (B,S) = s => {
      val (a,ss) = run(s)
      f(a).run(ss)
    }
    State(g)
  }

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] = {
    flatMap( a => sb.map( b => f(a,b)))
  }
}


object State {

}