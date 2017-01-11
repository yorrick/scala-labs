package yorrick.learningscalaz

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import JoulePerKiloGram._


class TaggedTypeTest extends FlatSpec with Matchers with OptionValues with TryValues {
  "type tags" should "be represented by any undelying types" in {
    // this does not compile: type tags provide type safety
//    energyR(20) shouldBe 1.79751035747363533E18

    // for energyR method, a Kilogram is represented by a Double
    energyR(KiloGram(20.0)) shouldBe 1.79751035747363533E18

    // for energyRInt method, a Kilogram is represented by an Int
    energyRInt(KiloGram(20)) shouldBe 1.79751035747363533E18

    // for energyRIntString method, a Kilogram is represented by an Int,
    // and the result JoulePerKiloGram can be represented by a String
    energyRIntString(KiloGram(20)) shouldBe "1.79751035747363533E18"
  }
}
