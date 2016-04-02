package yorrick.avro

import java.io.ByteArrayOutputStream

import com.gensler.scalavro.types.AvroType
import spray.json.JsValue


object Test {
  def toJson(user: User): JsValue = {
    val userType = AvroType[User]
    userType.io writeJson user
  }

  def toBinary(user: User): Array[Byte] = {
    val userType = AvroType[User]
    val os = new ByteArrayOutputStream()
    userType.io.write(user, os)
    os.toByteArray
  }
}
