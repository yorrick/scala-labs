package yorrick.eventsourcing2.candy


sealed trait Input
case object Coin extends Input
case object Turn extends Input
