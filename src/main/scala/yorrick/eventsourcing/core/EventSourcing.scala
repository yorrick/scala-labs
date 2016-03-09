package yorrick.eventsourcing.core

object EventSourcing {
  abstract class Aggregation
  abstract class Command
  // TODO make event reversible, with a null element

  abstract class Event {
    /** Command that created the event **/
    val command: Command
  }

  trait Handler[A <: Aggregation] {
    def diff(command: Command, state: Option[A]): Option[Event]
    def applyEvent(domainObject: Option[A], event: Event): A
  }

  private def processSingle[C <: Command, A <: Aggregation](domainObject: Option[A], command: C)(implicit handler: Handler[A]): (Option[A], Option[Event]) = {
    val eventOpt = handler.diff(command, domainObject)

    val resultingDomainObject: Option[A] = eventOpt match {
      case Some(event) => Some(handler.applyEvent(domainObject, event))
      case None => domainObject
    }

    (resultingDomainObject, eventOpt)
  }

  /**
   * Processes N commands, starting from domain object
   * @param domainObject
   * @param commands
   * @param handler
   * @tparam C
   * @tparam A
   * @return
   */
  def process[C <: Command, A <: Aggregation](domainObject: Option[A], commands: C*)(implicit handler: Handler[A]): Seq[(Option[A], Option[Event])] = {
    if (commands.isEmpty) {
      Seq.empty
    } else {
      val (obj, event) = processSingle(domainObject, commands.head)
      Seq((obj, event)) ++ process(obj, commands.tail: _*)
    }
  }
}
