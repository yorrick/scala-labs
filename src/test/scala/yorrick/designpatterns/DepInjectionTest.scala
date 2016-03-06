package yorrick.designpatterns

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import org.scalamock.scalatest.MockFactory
import DepInjection._


class DepInjectionTest extends FlatSpec with Matchers with OptionValues with TryValues with MockFactory {

  "depedency injection" should "be able to inject dependencies at compile time" in {
    object DefaultUserService 
      extends DefaultRepositoryComponent 
      with DefaultMessagingServiceComponent 
      with UserService {
        val repository = new DatabaseRepository
        val messagingService = new SimpleMessageService
      }

    DefaultUserService.create(User("toto")) shouldBe false
  }
  
  "depedency injection" should "be able to inject mocked dependencies at compile time" in {
    object DefaultUserService
      extends DefaultRepositoryComponent 
      with DefaultMessagingServiceComponent 
      with UserService {
        val repository = stub[Repository]
        val messagingService = new SimpleMessageService

        // always return true
        (repository.save _).when(*).returns(true)
      }

    DefaultUserService.create(User("toto")) shouldBe true
  }
}
