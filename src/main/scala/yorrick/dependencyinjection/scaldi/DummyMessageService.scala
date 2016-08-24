package yorrick.dependencyinjection.scaldi


class DummyMessageService extends MessageService {
   def getGreetMessage(name: String) = s"Yo, $name!"
 }
