package yorrick.designpatterns

object DepInjection {
  case class User(name: String)
  
  trait Repository {
    protected def save(user: User)
  }
  
  trait MessagingService {
    protected def sendMessage(message: String)
  }

  trait DatabaseRepository extends Repository {
    protected def save(user: User) {
      println(s"Saving: $user")
    }
  }

  trait SimpleMessageService extends MessagingService {
    protected def sendMessage(message: String) {
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
