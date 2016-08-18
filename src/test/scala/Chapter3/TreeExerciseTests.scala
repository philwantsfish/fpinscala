package Chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeExerciseTests extends FlatSpec with Matchers {
  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  val tree2: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))

  "Exercise3.25" should "implement size" in {
    Tree.size(tree) shouldBe 7
  }

  "Exercise3.26" should "implement max" in {
    Tree.maximum(tree) shouldBe 4
  }

  "Exercise3.27" should "implement depth" in {
    Tree.depth(tree) shouldBe 3
    Tree.depth(tree2) shouldBe 4
  }

  "Exercise 3.28" should "implement map" in {
    val answer: Tree[Int] = Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5)))
    val addOne : Int => Int = v => v + 1
    Tree.map(tree)(addOne) shouldBe answer
  }

  it should "implement size" in {
    Tree.sizeViaFoldRight(tree) shouldBe 7
  }

  it should "implement max" in {
    Tree.maximumViaFoldRight(tree) shouldBe 4
  }

  it should "implement depth" in {
    Tree.depthViaFoldRight(tree) shouldBe 3
    Tree.depthViaFoldRight(tree2) shouldBe 4
  }



}
