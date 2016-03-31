package yorrick.eventsourcing2.candy


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
}
