package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import Strategy._


class StrategyTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "context" should "be able to use different strategies" in {
    new Context(multiply).use(2, 3) shouldBe 6
    new Context(add).use(2, 3) shouldBe 5
  }
}
