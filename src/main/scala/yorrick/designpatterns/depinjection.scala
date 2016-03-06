package yorrick.designpatterns

object DepInjection {
  case class User(name: String)
  
  trait Repository {
    def save(user: User)
  }
  
  trait MessagingService {
    def sendMessage(message: String)
  }

  trait DatabaseRepository extends Repository {
    def save(user: User) {
      println(s"Saving: $user")
    }
  }

  trait SimpleMessageService extends MessagingService {
    def sendMessage(message: String) {
      println(s"Message: $message")
    }
  }

  trait UserService { self: Repository with MessagingService => // requires Repository
    def create(user: User) {
      save(user)
      sendMessage(s"Created user $user")
    }
  }
}
