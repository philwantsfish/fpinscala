package Chapter5

import org.scalatest.{FlatSpec, Matchers}


class StreamTests extends FlatSpec with Matchers {
  "Stream" should "convert to list" in {
    val s = Stream(1,2,3,4)
    s.toList shouldBe List(1,2,3,4)
  }

  it should "implement take" in {
    val s = Stream(1,2,3,4,5)
    s.take(2).toList shouldBe List(1,2)
  }

  it should "implement drop" in {
    val s = Stream(1,2,3,4,5)
    s.drop(2).toList shouldBe List(3,4,5)
  }

  it should "implement takeWhile" in {
    val s = Stream(1,2,3,4,5)
    s.takeWhile(i => i<3).toList shouldBe List(1,2)
  }

  it should "implements exists that terminates early" in {
    val s = Stream(1,2,3,4,5)
    s.exists(a => a==2)
  }

  it should "implement forall" in {
    val s = Stream(1,2,3,4,5)
    s.forAll(a => a > 0) shouldBe true
    s.forAll(a => a > 1) shouldBe false
  }

  it should "implement takeWhile via foldRight" in {
    val s = Stream(1,2,3,4,5)
    s.takeWhileViaFoldRight(i => i<3).toList shouldBe List(1,2)
  }

  it should "implement filter" in {
    val s = Stream(1,2,3,4,5)
    val f: Int => Boolean = i => i%2 != 0
    s.filter(f).toList shouldBe List(1,3,5)
  }

  it should "implement constant" in {
    Stream.constant(2).take(3).toList shouldBe List(2,2,2)
  }

  it should "implement from" in {
    Stream.from(2).take(3).toList shouldBe List(2,3,4)
  }

  it should "implement fibs" in {
    Stream.fibs().take(7).toList shouldBe List(1,1,2,3,5,8,13)
  }

  it should "implement constant via unfold" in {
    Stream.constantViaUnfold(2).take(3).toList shouldBe List(2,2,2)
  }

  it should "implement from via unfold" in {
    Stream.fromViaUnfold(2).take(3).toList shouldBe List(2,3,4)
  }


//  it should "implement fibs via unfold" in {
//    Stream.fibsViaUnfold.take(7).toList shouldBe List(1,1,2,3,5,8,13)
//  }

  it should "implement take via unfold" in {
    val s = Stream(1,2,3,4,5)
    s.takeViaUnfold(2).toList shouldBe List(1,2)
  }

  it should "implement primes" in {
    Stream.primes.take(12).toList shouldBe List(1,2,3,5,7,11,13,17,19,23,29,31)
  }

  it should "implement tails" in {
    val s = Stream(1,2,3,4)
    s.tails.map(_.toList).toList shouldBe List(List(1, 2, 3, 4), List(2, 3, 4), List(3, 4), List(4))
  }

  it should "implement zipWith" in {
    val s = Stream(1,2,3,4)
    val u = Stream("a", "b", "c")
    val f: (Int, String) => String = (i,s) => i.toString + s
    s.zipWith(u)(f).toList shouldBe List("1a", "2b", "3c")
  }

  it should "implement zipAll" in {
    val s = Stream(1,2,3,4)
    val u = Stream("a", "b", "c")
    s.zipAll(u).toList shouldBe List(
      (Some(1), Some("a")),
      (Some(2), Some("b")),
      (Some(3), Some("c")),
      (Some(4), None))
  }

  it should "implement startsWith" in {
    val s = Stream(1,2,3,4)
    val u = Stream(1,2)
    val v = Stream(3,4)
    s.startsWith(u) shouldBe true
    s.startsWith(v) shouldBe false
  }


}
