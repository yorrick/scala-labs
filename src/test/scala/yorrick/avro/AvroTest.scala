package yorrick.avro

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import spray.json._
import Test._


class AvroTest extends FlatSpec with Matchers with OptionValues with TryValues {

  val jsonUser = """{"name":"name","favoriteNumber":{"int":2},"favoriteColor":null}"""
  
  val binaryUser = Array[Byte](8, 110, 97, 109, 101, 2, 4, 0)
  
  "user" should "be serializable in json" in {
    JsonParser(toJson(User("name", Some(2), None))) shouldBe JsonParser(jsonUser)
  }
  
  "user" should "be serializable in binary" in {
    toBinary(User("name", Some(2), None)) shouldBe binaryUser
  }
  
  "user" should "be unserializable from json" in {
    fromJson(jsonUser).success.value shouldBe User("name", Some(2), None)
  }
  
  "user" should "be unserializable from binary" in {
    fromBinary(binaryUser).success.value shouldBe User("name", Some(2), None)
  }
}
