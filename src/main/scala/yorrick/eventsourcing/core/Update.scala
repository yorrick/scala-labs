package yorrick.eventsourcing.core


case class Update[V](fieldName: String, oldValue: V, newValue: V) {
  def inverse: Update[V] = Update(fieldName, newValue, oldValue)
}


object Update {
  def fromValues[V](fieldName: String, oldValue: => V, newValue: => V): Option[Update[V]] = {
    if (oldValue != newValue) {
      Some(Update(fieldName, oldValue, newValue))
    } else {
      None
    }
  }
}
