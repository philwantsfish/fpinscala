package Chapter4

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}


class OptionTests extends FlatSpec with Matchers {

  "Option" should "implement filter" in {
    val isOne: Int => Boolean = i => i == 1
    val isNotOne: Int => Boolean = i => i != 1
    val s = Some(1)
    s.filter(isOne) shouldBe s
    s.filter(isNotOne) shouldBe None
  }

  it should "implement orElse" in {
    val s  = Some(1)
    val notS = Some(2)
    s.orElse(notS) shouldBe s
    None.orElse(notS) shouldBe notS
  }

  it should "implement getOrElse" in {
    val s  = Some(1)
    s.getOrElse(1) shouldBe 1
    None.getOrElse(2) shouldBe 2
  }

  it should "implement map" in {
    val f: Int => Int = i => i + 1
    Some(1).map(f) shouldBe Some(2)
  }

  it should "implement flatMap" in {
    val f: Int => Option[Int] = i => Some(i + 1)
    Some(1).flatMap(f) shouldBe Some(2)
  }

  it should "be able to calculate variance" in {
    val xs = Seq(1.0, 2.0, 3.0, 4.0)
    val o = Option.variance(xs)
    o shouldBe Some(1.25)
  }

  it should "sequence a list of options" in {
    val as = List(Some(1), Some(2), Some(3), Some(4))
    Option.sequence(as) shouldBe Some(List(1,2,3,4))
    Option.sequence2(as) shouldBe Some(List(1,2,3,4))
  }

  it should "implement traverse" in {
    val as = List("1", "2", "3")
    val f: String => Option[Int] = a => Try(a.toInt) match {
      case Success(v) => Some(v)
      case Failure(e) => None
    }
    Option.traverse(as)(f) shouldBe Some(List(1,2,3))
    Option.traverse2(as)(f) shouldBe Some(List(1,2,3))
  }

  it should "traverse should return none sometimes" in {
    val as = List("1", "2", "3a")
    val f: String => Option[Int] = a => Try(a.toInt) match {
      case Success(v) => Some(v)
      case Failure(e) => None
    }
    Option.traverse(as)(f) shouldBe None
  }


}
