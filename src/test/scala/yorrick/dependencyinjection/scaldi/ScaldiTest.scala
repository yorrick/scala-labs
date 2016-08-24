package yorrick.dependencyinjection.scaldi

import java.util
import com.typesafe.config.ConfigFactory
import org.scalatest.{TryValues, OptionValues, Matchers, FlatSpec}
import scaldi._
import scaldi.Identifier._
import scala.collection.JavaConversions._


class ScaldiTest extends FlatSpec with Matchers with OptionValues with TryValues with Injectable {
  "scaldi" should "inject simple values" in {
    class MessageModule extends Module {
      bind [MessageService] to new OfficialMessageService
      binding identifiedBy "yorrick.dependencyinjection.scaldi.OfficialMessageService.officialGreeting" to "Welcome"
    }

    implicit val injector = new MessageModule
    val messageService = inject[MessageService]

    messageService.getGreetMessage("toto") shouldBe s"Welcome, toto!"
  }

  "scaldi" should "inject values from typesafe config" in {
    val config = ConfigFactory.parseMap(mapAsJavaMap(Map(
      "yorrick.dependencyinjection.scaldi.OfficialMessageService.officialGreeting" -> "Welcome from typesafe"
    )).asInstanceOf[util.Map[String, _ <: AnyRef]])

    class MessageModule extends Module {
      bind [MessageService] to new OfficialMessageService
    }

    implicit val injector = TypesafeConfigInjector(config) :: new MessageModule

    val messageService = inject[MessageService]

    messageService.getGreetMessage("toto") shouldBe s"Welcome from typesafe, toto!"
  }

  "scaldi" should "be allow for dynamic implementation injection" in {
    val serviceName = if (math.random < 0.5) "OfficialMessageService" else "DummyMessageService"

    class MessageModule extends Module {
      binding identifiedBy "OfficialMessageService" to new OfficialMessageService
      binding identifiedBy "DummyMessageService" to new DummyMessageService
      binding identifiedBy "yorrick.dependencyinjection.scaldi.OfficialMessageService.officialGreeting" to "Welcome"
    }

    implicit val injector = new MessageModule

    val messageService = inject[MessageService](identified by serviceName)

    messageService.getGreetMessage("toto") contains "toto" shouldBe true
  }

  "scaldi" should "be allow for dynamic implementation injection with typesafe config" in {
    val config = ConfigFactory.parseString(
      s"""
         |yorrick.dependencyinjection.scaldi {
         |  OfficialMessageService.officialGreeting: "Welcome"
         |  messageService: "yorrick.dependencyinjection.scaldi.OfficialMessageService"
         |}
       """.stripMargin)

    class MessageModule extends Module {
      binding identifiedBy "yorrick.dependencyinjection.scaldi.OfficialMessageService" to new OfficialMessageService
      binding identifiedBy "yorrick.dependencyinjection.scaldi.DummyMessageService" to new DummyMessageService
    }

    implicit val injector = TypesafeConfigInjector(config) :: new MessageModule

    val messageService = inject[MessageService](identified by inject[String](identified by "yorrick.dependencyinjection.scaldi.messageService"))

    messageService.getGreetMessage("toto") contains "toto" shouldBe true
  }

  "scaldi" should "be allow for dynamic injection of object list with typesafe config" in {
    val config = ConfigFactory.parseString(
      s"""
         |yorrick.dependencyinjection.scaldi {
         |  OfficialMessageService.officialGreeting: "Welcome"
         |  messageServices: [
         |    "yorrick.dependencyinjection.scaldi.OfficialMessageService"
         |    "yorrick.dependencyinjection.scaldi.DummyMessageService"
         |  ]
         |}
       """.stripMargin)

    class MessageModule extends Module {
      binding identifiedBy "yorrick.dependencyinjection.scaldi.OfficialMessageService" to new OfficialMessageService
      binding identifiedBy "yorrick.dependencyinjection.scaldi.DummyMessageService" to new DummyMessageService
    }

    implicit val injector = TypesafeConfigInjector(config) :: new MessageModule

    val identifiers: List[Identifier] =
      inject[List[String]](identified by "yorrick.dependencyinjection.scaldi.messageServices").map { s: String =>
        toIdentifier(s)
      }

    val messageServices = injectAllOfType[MessageService](identifiers: _*)

    messageServices.forall(ms => ms.getGreetMessage("toto") contains "toto") shouldBe true
  }
}
