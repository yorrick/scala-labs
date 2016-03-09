package yorrick.eventsourcing.core

object EventSourcing {
  abstract class Aggregation
  abstract class Command
  abstract class Event

  trait DiffCalculator[-C <: Command, -A <: Aggregation] {
    def diff(command: C, existingObject: Option[A]): Option[Event]
  }
  
  trait Handler[A <: Aggregation] {
    def diff(command: Command, state: Option[A]): Option[Event]
    def applyEvent(domainObject: Option[A], event: Event): A
    def getDomainObject(command: Command): Option[A]
    def saveDomainObject(domainObject: A): A
  }

  def process[C <: Command, A <: Aggregation](command: C)(implicit handler: Handler[A]): Option[Event] = {
    val domainObject = handler.getDomainObject(command)
    val eventOpt = handler.diff(command, domainObject)

    val newDomainObject = eventOpt.map { event =>
      handler.applyEvent(domainObject, event)
    }

    newDomainObject.foreach(handler.saveDomainObject)

    eventOpt
  }
}
