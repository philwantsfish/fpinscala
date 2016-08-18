package Chapter6

import org.scalatest.{FlatSpec, Matchers}

class MachineTests extends FlatSpec with Matchers {
  "A coin operation" should "unlock a locked machine" in {
    val m = Machine(true, 10, 0)
    val ((candies, coins), m1) = Machine.simulateSingleOperation(Coin).run(m)

    m1.locked shouldBe false
    coins shouldBe 1
    candies shouldBe 10
  }

  it should "do nothing to an unlocked machine" in {
    val m = Machine(false, 10, 0)
    val ((candies, coins), m1) = Machine.simulateSingleOperation(Coin).run(m)
    m shouldBe m1
    coins shouldBe 0
    candies shouldBe 10
  }

  "A turn operation" should "do nothing to a locked machine" in {
    val m = Machine(true, 10, 0)
    val ((candies, coins), m1) = Machine.simulateSingleOperation(Turn).run(m)
    m shouldBe m1
    candies shouldBe 10
    coins shouldBe 0
  }

  it should "take a candy to an unlocked machine" in {
    val m = Machine(false, 10, 1)
    val ((candies, coins), m1) = Machine.simulateSingleOperation(Turn).run(m)
    m1.locked shouldBe true
    candies shouldBe 9
    coins shouldBe 1
  }

  "A state machine" should "operate" in {
    val m = Machine(true, 10, 0)
    val operations = List(Turn, Coin, Coin, Turn, Coin, Turn)
    val ((candies, coins), finalState) = Machine.simulateMachine(operations).run(m)
    finalState.locked shouldBe true
    candies shouldBe 8
    coins shouldBe 2
  }
}
