package Chapter6

sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def unlock: Machine = Machine(false, candies, coins)
  def lock: Machine = Machine(true, candies, coins)
  def addCoin: Machine = Machine(locked, candies, coins+1)
  def takeCandy: Machine = Machine(locked, candies-1, coins)
  def isEmpty: Boolean = candies <= 0
}

object Machine {
  type Operation = Machine => ((Int, Int), Machine)

  def unit: Operation = machine => ((machine.candies, machine.coins), machine)

  def coinOperation: Operation = machine => {
      if(machine.isEmpty || !machine.locked) unit(machine)
      else unit(machine.unlock.addCoin)
    }

  def turnOperation: Operation = machine => {
    if(machine.isEmpty || machine.locked) unit(machine)
    else unit(machine.takeCandy.lock)
  }

  def simulateSingleOperation(input: Input): State[Machine, (Int,Int)] = {
    val f: Machine => ((Int,Int), Machine) = m => {
      input match {
        case Coin => coinOperation(m)
        case Turn => turnOperation(m)
      }
    }
    State(f)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    // Give a list of actions, convert this into a series of state transitions
    val stateTransitions: List[Machine => Machine] = inputs.map { i =>
      (machine: Machine) => simulateSingleOperation(i).run(machine)._2
    }

    // Chain the state transitions together and execute them with an initial state
    State((m: Machine) => {
      val finalState: Machine = Function.chain(stateTransitions)(m)
      unit(finalState)
    })
  }
}