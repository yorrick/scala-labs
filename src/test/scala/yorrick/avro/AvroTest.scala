package yorrick.avro

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import spray.json._
import Test._


class AvroTest extends FlatSpec with Matchers with OptionValues with TryValues {

  val jsonUser = """{"name":"name","favoriteNumber":{"int":2},"favoriteColor":null}"""
  
  "user" should "be serializable in json" in {
    toJson(User("name", Some(2), None)) shouldBe JsonParser(jsonUser)
  }
  
  "user" should "be serializable in binary" in {
    toBinary(User("name", Some(2), None)) shouldBe Array[Byte](8, 110, 97, 109, 101, 2, 4, 0)
  }
}
