package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}


class FactoryTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "animal factory" should "be able to build cats" in {
    Animal("cat") shouldBe a [Cat]
  }
}
