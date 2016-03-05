package yorrick.designpatterns

import java.util.logging.Level
import java.util.logging.Level.{WARNING, INFO}


trait Log {
  def warning(message: String)
  def info(message: String)
}

final class Logger {
  def log(level: Level, message: String) {
    println(s"$level: $message")
  }
}

object LoggerAdapter {

  implicit class LoggerToLogAdapter(logger: Logger) extends Log {
    def warning(message: String) {
      logger.log(WARNING, message)
    }

    def info(message: String) {
      logger.log(INFO, message)
    }
  }

}
