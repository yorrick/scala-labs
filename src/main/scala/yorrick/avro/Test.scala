package yorrick.avro

import com.gensler.scalavro.types.AvroType
import spray.json.JsValue


object Test {
  def toAvroUser(user: User): JsValue = {
    val userType = AvroType[User]
    userType.io writeJson user
  }
}
