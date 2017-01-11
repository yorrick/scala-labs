package yorrick.learningscalaz

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}

import scalaz.Order.order
import scalaz.Scalaz._
import scalaz.{Monoid => SZMonoid, _}


class OptionMonoidTest extends FlatSpec with Matchers with OptionValues with TryValues {
  "scalaz option monoid" should "use inner monoid" in {
    (none: Option[String]) |+| "andy".some shouldBe Some("andy")

    // here we can see that string concatenation is used: option uses String monoid
    "tily".some |+| "andy".some shouldBe Some("tilyandy")
  }

  "scalaz option monoid" should "allow to customize monoid behaviour" in {
    // here we can see that first option is used, using "First" tag
    Tags.First("tily".some) |+| Tags.First("andy".some) shouldBe Some("tily")

    Tags.Last('a'.some) |+| Tags.Last('b'.some) shouldBe Some('b')
  }
}
