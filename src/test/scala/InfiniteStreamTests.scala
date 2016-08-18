import org.scalatest.{Matchers, FlatSpec}

class InfiniteStreamTests extends FlatSpec with Matchers {
  import InfiniteStreams._

  "fib" should "implement the fibonacci sequence" in {
    fib.take(10) shouldBe Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  "primes" should "implement a stream of prime numebrs" in {
    primes.take(10) shouldBe Stream(1, 2, 3, 5, 7, 11, 13, 17, 19, 23)
  }
}