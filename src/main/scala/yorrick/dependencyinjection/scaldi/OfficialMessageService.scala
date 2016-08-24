package yorrick.dependencyinjection.scaldi

import scaldi.{Injectable, Injector}

class OfficialMessageService(implicit inj: Injector) extends MessageService with Injectable {
  val officialGreeting = inject[String] (identified by "yorrick.dependencyinjection.scaldi.OfficialMessageService.officialGreeting")
  def getGreetMessage(name: String) = s"$officialGreeting, $name!"
}
