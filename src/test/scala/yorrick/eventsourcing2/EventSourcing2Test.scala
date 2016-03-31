package yorrick.eventsourcing2

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import yorrick.eventsourcing2.candy.{Input, Coin, Turn, Machine}
import yorrick.eventsourcing2.candy.Candy.update
import yorrick.eventsourcing2.core.Action._


class EventSourcing2Test extends FlatSpec with Matchers with OptionValues with TryValues {
  val candiesSystem = applyInputs[Input, Machine](update)_

  "Turn on an unlocked machine with candies and coins" should "decrease the number of candies" in {
    candiesSystem(Machine(false, 10, 3))(Seq(Turn)) shouldBe Machine(true, 9, 3)
  }
  
  "Inputs on an locked machine with no candies" should "do nothing" in {
    candiesSystem(Machine(false, 0, 3))(Seq(Turn, Coin, Turn, Coin)) shouldBe Machine(false, 0, 3)
  }
  
  "Putting coins on an unlocked machine with candies and coins" should "do nothing" in {
    candiesSystem(Machine(false, 10, 3))(Seq(Coin, Coin, Coin)) shouldBe Machine(false, 10, 3)
  }
  
  "Turn Coin Sequence on an unlocked machine with candies and coins" should "increase the number of coins, and decrease the number of candies" in {
    candiesSystem(Machine(false, 10, 3))(Seq(Turn, Coin, Turn, Coin)) shouldBe Machine(false, 8, 5)
  }
}
