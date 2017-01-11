package yorrick.learningscalaz

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import scalaz._
import Scalaz._
import Order.order
import scalaz.{Monoid => SZMonoid}


class MonoidTest extends FlatSpec with Matchers with OptionValues with TryValues {
  "scalaz monoids" should "define tags to choose between monoids" in {
    // simplest case: default Monoid[Int] represents addition
    SZMonoid[Int].zero shouldBe 0
    10 |+| SZMonoid[Int].zero shouldBe 10

    // we can choose among monoids bt using Tags
    SZMonoid[Int @@ Tags.Multiplication].zero shouldBe 1
    Tags.Multiplication(10) |+| SZMonoid[Int @@ Tags.Multiplication].zero shouldBe 10
  }

  "scalaz Ordering monoid" should "allow for multiple levels of comparison" in {
    def lengthCompare(lhs: String, rhs: String): Ordering = (lhs.length ?|? rhs.length) |+| (lhs ?|? rhs)

    lengthCompare("zen", "ants") shouldBe Ordering.LT
    lengthCompare("zen", "ant") shouldBe Ordering.GT

    case class Person(age: Int, name: String, other: Int = 0)

    def personCompare(p1: Person, p2: Person): Ordering =
      (p1.age ?|? p2.age) |+| (p1.name ?|? p2.name) |+| (p1.hashCode() ?|? p2.hashCode())

    personCompare(Person(10, "toto"), Person(12, "toto")) shouldBe Ordering.LT
    personCompare(Person(13, "toto"), Person(12, "toto")) shouldBe Ordering.GT

    personCompare(Person(12, "tota"), Person(12, "toto")) shouldBe Ordering.LT
    personCompare(Person(12, "toto"), Person(12, "toto")) shouldBe Ordering.EQ
    personCompare(Person(12, "toto", 2), Person(12, "toto")) shouldBe Ordering.GT

    // define Order[Person]
    implicit val personOrder: Order[Person] = order(personCompare)

    Person(13, "toto") ?|? Person(12, "toto") shouldBe Ordering.GT

    // define scala ordering to be able to use sorted
    implicit val personOrdering = personOrder.toScalaOrdering
    Seq(Person(13, "toto"), Person(12, "toto")).sorted shouldBe Seq(Person(12, "toto"), Person(13, "toto"))
  }
}
