package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import DepInjection._


class DepInjectionTest extends FlatSpec with Matchers with OptionValues with TryValues {

  "depedency injection" should "be able to inject dependencies at compile time" in {
    object DefaultUserService 
      extends DefaultRepositoryComponent 
      with DefaultMessagingServiceComponent 
      with UserService {
        val repository = new DatabaseRepository
        val messagingService = new SimpleMessageService
      }

    DefaultUserService.create(User("toto"))
  }
}
