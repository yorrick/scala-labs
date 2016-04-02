package yorrick.avro

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import spray.json._
import Test._


class AvroTest extends FlatSpec with Matchers with OptionValues with TryValues {

  val jsonUser = """{"name":"name","favoriteNumber":{"int":2},"favoriteColor":null}"""
  
  "avro user" should "return user" in {
    toAvroUser(User("name", Some(2), None)) shouldBe JsonParser(jsonUser)
  }
}
