package Chapter8

import Chapter6.{RNG, State}
import Chapter8.Prop.{FailedCase, SuccessCount}


object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

case class Gen[A](sample: State[RNG, A]) {
  def unit(a: => A): Gen[A] = Gen(State(s => (a, s)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???
}

object chapter8 {


}
