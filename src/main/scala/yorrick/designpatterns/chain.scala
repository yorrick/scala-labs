package yorrick.designpatterns

  
object Chain {
  case class Event(source: String)

  type EventHandler = PartialFunction[Event, Option[String]]

  val defaultHandler: EventHandler = PartialFunction(_ => None)

  val keyboardHandler: EventHandler = {
    case Event("keyboard") => Some("keyboard")
  }

  def mouseHandler(delay: Int): EventHandler = {
    case Event("mouse") => Some("mouse")
  }
}
