package Chapter6

import org.scalatest.{FlatSpec, Matchers}


class RNGTests extends FlatSpec with Matchers {
  "RNG" should "implement ints" in {
    val rng = SimpleRNG(1337)
    rng.ints(4)(rng)._1.length shouldBe 4
    rng.ints(4)(rng)._1 shouldBe rng.ints(4)(rng)._1
  }

  it should "implement double via map" in {
    val rng = SimpleRNG(1337)
    rng.double(rng) shouldBe rng.doubleViaMap(rng)
  }

  it should "implement ints via sequence" in {
    val rng = SimpleRNG(1337)
    rng.ints(4)(rng) shouldBe rng.intsViaSequence(4)(rng)
  }



}
