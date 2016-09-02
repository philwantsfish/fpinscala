package Chapter10

import org.scalatest.{FlatSpec, Matchers}


class MonoidTests extends FlatSpec with Matchers
{
  "Monoid" should "do something cool" in {
    println("hey")
    val good = IndexedSeq(1,2,3,4)
    val bad = IndexedSeq(1,2,3,2)

    Monoids.isIndexedSeqOrdered(good) shouldBe true
    Monoids.isIndexedSeqOrdered(bad) shouldBe false
  }

}
