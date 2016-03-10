package yorrick.eventsourcing.core


// TODO make backups
// TODO rebuild state
// TODO build diffs dynamically using introspection
// TODO make event reversible, with a null element
object EventSourcing {
  abstract class Aggregation

  abstract class Command {
    final def inverse: Command = InversedCommand(this)
  }
  
  object EmptyCommand extends Command
  case class InversedCommand(command: Command) extends Command

  abstract class Event {
    /** Command that created the event **/
    val command: Command
    
    def inverse: Event
  }

  trait Handler[A <: Aggregation] {
    def diff(command: Command, state: Option[A]): Option[Event]
    def applyEvent(domainObject: Option[A], event: Event): Option[A]
  }

  private def processSingle[C <: Command, A <: Aggregation](domainObject: Option[A], command: C)(implicit handler: Handler[A]): (Option[A], Option[Event]) = {
    val eventOpt = handler.diff(command, domainObject)

    val resultingDomainObject: Option[A] = eventOpt match {
      case Some(event) => handler.applyEvent(domainObject, event)
      case None => domainObject
    }

    (resultingDomainObject, eventOpt)
  }

  /**
   * Processes N commands, starting from domain object
   * TODO check for tail recursion optimization 
   * @param domainObject
   * @param commands
   * @param handler
   * @tparam C
   * @tparam A
   * @return
   */
  def processCommands[C <: Command, A <: Aggregation](domainObject: Option[A], commands: C*)(implicit handler: Handler[A]): Seq[(Option[A], Option[Event])] = commands.toList match {
    case Nil => Seq.empty
    case command :: tail => {
      val (obj, event) = processSingle(domainObject, commands.head)
      Seq((obj, event)) ++ processCommands(obj, commands.tail: _*)
    }
  }

  /**
   * Applies N events to an object
   * @param domainObject
   * @param events
   * @param handler
   * @tparam A
   * @return
   */
  def processEvents[A <: Aggregation](domainObject: Option[A], events: Event*)(implicit handler: Handler[A]): Option[A] = events.toList match {
    case Nil => domainObject
    case event :: Nil => handler.applyEvent(domainObject, event)
    case event :: tail => processEvents(handler.applyEvent(domainObject, event), tail: _*)
  }
}
