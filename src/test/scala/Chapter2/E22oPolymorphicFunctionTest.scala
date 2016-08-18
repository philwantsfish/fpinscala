package Chapter2

import org.scalatest.{FlatSpec, Matchers}


class E22oPolymorphicFunctionTest extends FlatSpec with Matchers {
  "isSorted" should "pass for an array of sorted integers" in {
    val sortedInts = Array(1, 2, 3, 4)
    val checkSort: (Int, Int) => Boolean = (x, y) => x <= y
    E22oPolymorphicFunction.isSorted(sortedInts, checkSort) shouldBe true
  }

  "isSorted" should "fail for an array of unsorted integers" in {
    val sortedInts = Array(1, 2, 3, 5, 4)
    val checkSort: (Int, Int) => Boolean = (x, y) => x <= y
    E22oPolymorphicFunction.isSorted(sortedInts, checkSort) shouldBe false
  }

  "isSorted2" should "pass for an array of sorted integers" in {
    val sortedInts = Array(1, 2, 3, 4)
    val checkSort: (Int, Int) => Boolean = (x, y) => x <= y
    E22oPolymorphicFunction.isSorted2(sortedInts, checkSort) shouldBe true
  }

  "isSorted2" should "fail for an array of unsorted integers" in {
    val sortedInts = Array(1, 2, 3, 5, 4)
    val checkSort: (Int, Int) => Boolean = (x, y) => x <= y
    E22oPolymorphicFunction.isSorted2(sortedInts, checkSort) shouldBe false
  }

  "isSorted2" should "pass for an array of sorted string lengths" in {
    val sortedInts = Array("A", "AA", "AAA")
    val checkSort: (String, String) => Boolean = (x, y) => x.length <= y.length
    E22oPolymorphicFunction.isSorted2(sortedInts, checkSort) shouldBe true
  }

  "isSorted2" should "fail for an array of unsorted string lengths" in {
    val sortedInts = Array("A", "AAA", "AA")
    val checkSort: (String, String) => Boolean = (x, y) => x.length <= y.length
    E22oPolymorphicFunction.isSorted2(sortedInts, checkSort) shouldBe false
  }
}
