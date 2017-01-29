package yorrick.macros

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import yorrick.macros.to_string.DataToString


@DataToString
case class Person(name: String, age: Int)
@DataToString
case class Person2(name2: String, age: Int, age2: Double)


class DataToStringTest extends FlatSpec with Matchers with OptionValues with TryValues {
  "DataToStringTest annotation" should "make animals talk" in {
    Person("Jack", 2).dataToString shouldBe "Jack2"
  }

}
