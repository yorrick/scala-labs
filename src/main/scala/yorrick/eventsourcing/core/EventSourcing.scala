package yorrick.eventsourcing.core

object EventSourcing {
  abstract class Aggregation
  abstract class Command
  // TODO make event reversible, with a null element
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

//  TODO remove side effects
  def process[C <: Command, A <: Aggregation](command: C)(implicit handler: Handler[A]): (Option[Event], Option[A]) = {
    val domainObject = handler.getDomainObject(command)
    val eventOpt = handler.diff(command, domainObject)

    val resultingDomainObject: Option[A] = eventOpt match {
      case Some(event) => Some(handler.applyEvent(domainObject, event))
      case None => domainObject
    }

    resultingDomainObject.foreach(handler.saveDomainObject)

    (eventOpt, resultingDomainObject)
  }
}
