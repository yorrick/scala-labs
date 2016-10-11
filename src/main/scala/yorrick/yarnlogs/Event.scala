package yorrick.yarnlogs

import java.util.Date

case class Event(date: Date, level: String, message: String, exception: Option[Error])
