package Chapter3

import Chapter3.List._
import org.scalatest.{Matchers, FlatSpec}


class ListExerciseTests extends FlatSpec with Matchers {
  "Exercise3.1" should "print the answer" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)
  }

  "Exercise3.2" should "List.tail will remove the head" in {
    val l = List(1, 2, 3)
    val tail = List(2, 3)

    List.tail(l) shouldBe tail
  }

  "Exercise 3.3" should "List.setHead will modify the head" in {
    val l = List(1, 2, 3)
    val answer = List(4, 2, 3)

    List.setHead(l, 4) shouldBe answer
  }

  "Exercise 3.4" should "List.drop generalizes tail" in {
    val l = List(1, 2, 3)
    val answer = List(3)

    List.drop(l, 2) shouldBe answer
  }

  it should "List.drop return Nil when dropping more than the list size" in {
    val l = List(1, 2, 3)
    val answer = Nil

    List.drop(l, 10) shouldBe answer
  }

  "Exercise 3.5" should "List.dropWhile will drop until predicate" in {
    val f: Int => Boolean = x => x % 2 == 0
    val l = List(2,4,6,8,9)
    val answer = List(9)
    List.dropWhile(l, f) shouldBe answer
  }

  "Exercise 3.6" should "List.init should create a list without the last element" in {
    val l = List(1,2,3,4)
    val answer = List(1,2,3)
    List.init(l) shouldBe answer
  }

  "Exercise 3.8" should "investigate what happens" in {
    val a = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    println(a)
  }

  "Exercise 3.9" should "List.length should work" in {
    val l = List(1,2,3,4)
    val answer = 4
    List.length(l) shouldBe answer
  }

  "Exercise 3.10" should "Prove foldRight is not tail recursive" in {
    """I don't think we can short circuit foldRight without modifying the function to take an additional argument.
      | Implement it something along the lines of dropWhile""".stripMargin
  }

  "Exercise 3.11" should "Write sum using foldLeft" in {
    val l = List.sumLeft(List(1,2,3))
    val answer = 6
    l shouldBe answer
  }

  it should "Write product using foldLeft" in {
    val l = List.productLeft(List(1,2,3,4))
    val answer = 24
    l shouldBe answer
  }

  it should "Write length using foldLeft" in {
    val l = List.lengthLeft(List(1,2,3,4,5,6))
    val answer = 6
    l shouldBe answer
  }

  "Exercise 3.12" should "Write reverse using a fold" in {
    val l = List(1,2,3)
    val answer = List(3,2,1)
    List.reverse(l) shouldBe answer
  }

  "Exercise 3.13" should "Write foldLeft in terms of foldRight" in {
    """Not sure how to do this"""
  }


  it should "Write foldRight in terms of foldLeft" in {
    """Not sure how to do this"""
  }

  "Exercise3.14" should "Implement append with foldRight" in {
    val l = List(1,2,3)
    val answer = List(1,2,3,4)
    List.appendRight(l, 4) shouldBe answer
  }

  "Exercise3.15" should "Needs a helper function to combine two lists" in {
    val a = List(1,2)
    val b = List(3,4)
    val answer = List(1,2,3,4)
    List.appendList(a, b) shouldBe answer
  }

  it should "Implement concat a List of Lists" in {
    val a = List(1,2)
    val b = List(3,4)
    val c = List(5,6)
    val answer = List(1,2,3,4,5,6)
    List.concatLists(List(a,b,c)) shouldBe answer
  }

  "Exercise 3.16" should "Implement add by one" in {
    val a = List(1,2,3)
    val answer = List(2,3,4)
    List.intAddOne(a) shouldBe answer
  }

  "Exercise 3.17" should "Implement converting list of doubles to strings" in {
    val a = List(1.0, 2.0, 3.0)
    val answer = List("1.0", "2.0", "3.0")
    List.doubleToStringList(a) shouldBe answer
  }

  "Exercise 3.18" should "Implement map" in {
    val as = List(1,2,3)
    val answer = List(2,3,4)
    List.map(as)((a:Int) => a+1) shouldBe answer
  }

  "Exercise 3.19" should "Implement filter" in {
    val a = List(1,2,3,4,5,6)
    val answer = List(1,3,5)
    List.filter(a)((a:Int) => a % 2 != 0) shouldBe answer
  }

  "Exercise 3.20" should "Implement flatMap" in {
    val a = List(1,2,3)
    val answer = List(1,1,2,2,3,3)
    List.flatMap(List(1,2,3))(i => List(i,i)) shouldBe answer
  }

  "Exercise 3.21" should "Implemented filter via flatMap" in {
    val a = List(1,2,3,4,5,6)
    val answer = List(1,3,5)
    List.filterViaFlatMap(a)((a:Int) => a % 2 != 0) shouldBe answer
  }

  "Exercise 3.22" should "Implement zip two lists of ints" in {
    val as = List(1,2,3)
    val bs = List(4,5,6)
    val answer = List((1,4), (2,5), (3,6))
    List.zipInts(as, bs) shouldBe answer
  }

  it should "Implment add two list of ints" in {
    val as = List(1,2,3)
    val bs = List(4,5,6)
    val answer = List(5,7,9)
    List.addListElements(as, bs) shouldBe answer
  }

  "Exercise 3.23" should "Implement zipWith" in {
    val as = List(1,2,3)
    val bs = List(4,5,6)
    val f: (Int,Int) => Int = (a,b) => a+b
    val answer = List(5,7,9)
    List.zipWith(as, bs)(f) shouldBe answer
  }

  "Exercise 3.24" should "Implement hasSubsequence" in {
    """Didnt implement"""
  }
}
