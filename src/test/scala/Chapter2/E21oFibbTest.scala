package Chapter2


import org.scalatest.{FlatSpec, Matchers}


class E21oFibbTest extends FlatSpec with Matchers {
  "Fib sequence" should "give proper results" in {
    E21oFibb.fib(0) shouldBe 0
    E21oFibb.fib(1) shouldBe 1
    E21oFibb.fib(2) shouldBe 1
    E21oFibb.fib(3) shouldBe 2
    E21oFibb.fib(4) shouldBe 3
    E21oFibb.fib(5) shouldBe 5
    E21oFibb.fib(6) shouldBe 8
  }

  "Fib sequence improved" should "give proper results" in {
    E21oFibb.fib2(0) shouldBe 0
    E21oFibb.fib2(1) shouldBe 1
    E21oFibb.fib2(2) shouldBe 1
    E21oFibb.fib2(3) shouldBe 2
    E21oFibb.fib2(4) shouldBe 3
    E21oFibb.fib2(5) shouldBe 5
    E21oFibb.fib2(6) shouldBe 8
  }
}
