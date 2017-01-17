package yorrick.learningscalaz

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import JoulePerKiloGram._

import scalaz._
import Scalaz._


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

  "type tag" should "be usable with Ordering type class" in {
//    def kgCompare(kg1: Int @@ KiloGram, kg2: Int @@ KiloGram): Ordering = Tag.unwrap(kg1) ?|? Tag.unwrap(kg2)
//    implicit def tagOrder: Order[@@[Int, KiloGram]] = Order.order(Tag.unwrap(_) ?|? Tag.unwrap(_))

    implicit def tagOrder[V: Order, T]: Order[@@[V, T]] = Order.order(Tag.unwrap(_) ?|? Tag.unwrap(_))

    KiloGram(21) ?|? KiloGram(20) shouldBe Ordering.GT
  }
}
