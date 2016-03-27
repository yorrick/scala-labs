package yorrick.eventsourcing.core

import Action._
import Candy._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)


object Candy {

  /**
   * From an input, builds a state transition (Machine => Machine function)
   */
  def update: Input => (Machine => Machine) = input => machine => (input, machine) match {
    case (_, Machine(_, 0, _)) => machine
    case (Coin, Machine(false, _, _)) => machine
    case (Turn, Machine(true, _, _)) => machine
    case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
  }

  def applyInputs(machine: Machine)(inputs: Input*): Machine = {
    applyUpdates(machine)(update)(inputs.toList)
  }
}


object Test {

  def main(args: Array[String]): Unit = {
    println(applyInputs(Machine(false, 10, 3))(Turn))
    println(applyInputs(Machine(false, 0, 3))(Turn, Coin, Turn, Coin))
    println(applyInputs(Machine(false, 10, 3))(Coin, Coin, Turn, Coin))
    println(applyInputs(Machine(false, 10, 3))(Coin, Coin, Turn, Coin, Turn))
    println(applyInputs(Machine(true, 10, 3))(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
  }
}
