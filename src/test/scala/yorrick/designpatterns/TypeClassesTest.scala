package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import Statistics._
import Math._


class TypeClassesTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "type classes" should "use defined type classes" in {
    val numbers = Vector[Double](13, 23.0, 42, 45, 61, 73, 96, 100, 199, 420, 900, 3839)
    mean(numbers) shouldBe 484.25
  }

  "type classes" should "allow to extend API by defining new type classes" in {
    implicit object NumberLikeInt extends NumberLike[Int] {
      def plus(x: Int, y: Int): Int = x + y
      def divide(x: Int, y: Int): Int = x / y
      def minus(x: Int, y: Int): Int = x - y
    }

    val numbers = Vector[Int](1, 3, 5, 8, 4)
    mean(numbers) shouldBe 4  // value stays an integer (by API design)
  }
}
