package yorrick.dependencyinjection.scaldi

trait MessageService {
  def getGreetMessage(message: String): String
}
