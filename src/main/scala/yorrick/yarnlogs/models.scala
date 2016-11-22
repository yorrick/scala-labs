package yorrick.yarnlogs

import java.util.Date

trait Event

case class DefaultEvent(text: String) extends Event
case class LogEvent(date: Date, level: String, message: String, exception: Option[Error]) extends Event
case class Error(className: String, message: String)
