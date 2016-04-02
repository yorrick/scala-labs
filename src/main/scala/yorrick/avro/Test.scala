package yorrick.avro

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import com.gensler.scalavro.types.AvroType

import scala.util.Try


object Test {
  // TODO check if a def is necessary
  val userType = AvroType[User]
  
  def toJson(user: User): String = {
    userType.io writeJson user toString
  }

  def toBinary(user: User): Array[Byte] = {
    val os = new ByteArrayOutputStream()
    userType.io.write(user, os)
    os.toByteArray
  }
  
  def fromJson(content: String): Try[User] = {
    userType.io.readJson(content)
  }
  
  def fromBinary(content: Array[Byte]): Try[User] = {
    val is = new ByteArrayInputStream(content)
    userType.io.read(is)
  }
}
