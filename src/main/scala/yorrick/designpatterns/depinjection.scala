package yorrick.designpatterns

object DepInjection {
  case class User(name: String)

  trait RepositoryComponent {
    val repository: Repository
    
    trait Repository {
      def save(user: User): Boolean
    }
  }
  
  trait MessagingServiceComponent {
    val messagingService: MessagingService
    
    trait MessagingService {
      def sendMessage(message: String)
    }
  }

  trait DefaultRepositoryComponent extends RepositoryComponent {
    class DatabaseRepository extends Repository {
      def save(user: User): Boolean = if (user.name == "toto") false else true
    }
  }
  
  trait DefaultMessagingServiceComponent extends MessagingServiceComponent {
    class SimpleMessageService extends MessagingService {
      def sendMessage(message: String) {
        println(s"Message: $message")
      }
    }
  }

  // service declaring two dependencies that it wants injected
  trait UserService { this: MessagingServiceComponent with DefaultRepositoryComponent =>

    def create(user: User): Boolean = {
      val saveResult = repository.save(user)
      messagingService.sendMessage(s"Created user $user")
      saveResult
    }
  }
}
