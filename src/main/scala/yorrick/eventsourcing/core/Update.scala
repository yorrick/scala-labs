package yorrick.eventsourcing.core


case class Update[V](fieldName: String, oldValue: V, newValue: V)


object Update {
  def fromValues[V](fieldName: String, oldValue: => V, newValue: => V): Option[Update[V]] = {
    if (oldValue != newValue) {
      Some(Update(fieldName, oldValue, newValue))
    } else {
      None
    }
  }
}
