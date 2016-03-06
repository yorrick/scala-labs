package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import DepInjection._


class DepInjectionTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "depedency injection" should "be able to inject dependencies at compile time" in {
    val userService = new UserService with DatabaseRepository with SimpleMessageService
    userService.create(User("toto"))
  }
}
