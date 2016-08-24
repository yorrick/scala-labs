//package yorrick.avro
//
//import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
//import spray.json._
//import Test._
//
//
//// string middleName = ""; was added to schema in v2
//class AvroTest extends FlatSpec with Matchers with OptionValues with TryValues {
//  val jsonUserV1 = """{"name":"name","favoriteNumber":{"int":2},"favoriteColor":null}"""
//
//  val binaryUserV1 = Array[Byte](8, 110, 97, 109, 101, 2, 4, 0)
//
//  val jsonUserV2 = """{"name":"name","favoriteNumber":{"int":2},"favoriteColor":null,"middleName":""}"""
//
//  val binaryUserV2 = Array[Byte](8, 110, 97, 109, 101, 2, 4, 0, 0)
//
//  "user" should "be serializable in json using latest schema" in {
//    JsonParser(toJson(User("name", Some(2), None))) shouldBe JsonParser(jsonUserV2)
//  }
//
//  "user" should "be serializable in binary using latest schema" in {
//    toBinary(User("name", Some(2), None)) shouldBe binaryUserV2
//  }
//
////  "old version user" should "be unserializable from json using latest schema" in {
////    fromJson(jsonUserV1).get
////    fromJson(jsonUserV1).success.value shouldBe User("name", Some(2), None, middleName = "")
////  }
////
////  "old version user" should "be unserializable from binary using latest schema" in {
////    fromBinary(binaryUserV1).success.value shouldBe User("name", Some(2), None, middleName = "")
////  }
//
//  "latest version user" should "be unserializable from json using latest schema" in {
//    fromJson(jsonUserV2).success.value shouldBe User("name", Some(2), None)
//  }
//
//  "latest version user" should "be unserializable from binary using latest schema" in {
//    fromBinary(binaryUserV2).success.value shouldBe User("name", Some(2), None)
//  }
//}
