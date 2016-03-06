package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import Chain._


class ChainTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "chain" should "be able to use right handler" in {
    val chain = keyboardHandler.orElse(mouseHandler(100)).orElse(defaultHandler)
    chain(Event("keyboard")).value shouldBe "keyboard"
    chain(Event("mouse")).value shouldBe "mouse"
    chain(Event("toto")) shouldBe None
  }
}
