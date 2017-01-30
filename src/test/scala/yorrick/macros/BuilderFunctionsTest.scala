package yorrick.macros


import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import yorrick.macros.to_string.BuilderFunctions
import Converter._


object Converter {
  implicit def id[T]: T => T = identity
  implicit def valueToOption[T]: T => Option[T] = Option(_)
}


@BuilderFunctions
case class Person(name2: String = "", age: Int = 0, age2: Option[Double] = None, other: Seq[String] = Seq.empty) {
  def extraMethod = "toto"
}


class BuilderFunctionsTest extends FlatSpec with Matchers with OptionValues with TryValues {
  "BuilderFunctions annotation" should "add working builder functions on case classes" in {
    Person("Jack").age(20) shouldBe Person("Jack", 20)
  }

  "BuilderFunctions annotation" should "not alter existing methods" in {
    Person("Jack").extraMethod shouldBe "toto"
  }

}
