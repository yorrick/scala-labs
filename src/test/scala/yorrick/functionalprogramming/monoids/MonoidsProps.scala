package yorrick.functionalprogramming.monoids

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import Monoid._


class MonoidsProps extends PropSpec with PropertyChecks with Matchers {
  def monoidLaws[A](m: Monoid[A], message: String, gen: Gen[A]): Unit = property(s"monoid laws for $message") {
    // identity
    forAll(gen) { a =>
      m.op(a, m.zero) shouldBe a
      m.op(m.zero, a) shouldBe a
    }

    val tuple3Gen = for {
      a1 <- gen
      a2 <- gen
      a3 <- gen
    } yield (a1, a2, a3)

    // associativity
    forAll(tuple3Gen) { case (a1, a2, a3) =>
      m.op(m.op(a1, a2), a3) shouldBe m.op(a1, m.op(a2, a3))
    }
  }

  val offsetGen: Gen[Int => Int] = for {
    offset <- Gen.choose(-10, 10)
  } yield (input: Int) => input + offset

  monoidLaws(intAddition, "intAddition", Gen.choose(-10, 10))
  monoidLaws(intMultiplication, "intMultiplication", Gen.choose(-10, 10))
  monoidLaws(booleanOr, "booleanOr", Gen.oneOf(true, false))
  monoidLaws(booleanAnd, "booleanAnd", Gen.oneOf(true, false))
  monoidLaws(firstOptionMonoid[Int], "firstOptionMonoid", Gen.option(Gen.choose(-10, 10)))
  monoidLaws(lastOptionMonoid[Int], "lastOptionMonoid", Gen.option(Gen.choose(-10, 10)))

  // function equality does not work
//  monoidLaws(endoMonoid[Int], "endoMonoid", offsetGen)
}