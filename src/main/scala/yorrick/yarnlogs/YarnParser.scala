package yorrick.yarnlogs

import java.text.SimpleDateFormat
import com.jmolly.stacktraceparser.{NStackTrace, StackTraceParser}
import scala.util.Try
import collection.JavaConverters._
import Implicits._


object YarnParser {
  /** Returns shortest possible list of lists xss such that
    *   - xss.flatten == xs
    *   - No sublist in xss contains an element matching p in its tail
    */
  def groupPrefix[T](xs: List[T])(p: T => Boolean): List[List[T]] = xs match {
    case List() => List()
    case x :: xs1 =>
      val (ys, zs) = xs1 span (!p(_))
      (x :: ys) :: groupPrefix(zs)(p)
  }

  def grep(regexes: String*): String => Boolean = line => regexes.exists(regex => line.matches(regex))

  val container = """container_\d{13}_\d{4}_\d{2}_\d{6}"""
  val containerSeparator = s"$container.*".i
  val ContainerRegex = s""".*($container).*""".r

  val timestamp = """\d{2}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}"""
  val logEventSeparator = s"""^${timestamp} .*"""

  val df = new SimpleDateFormat("yy/MM/dd hh:mm:ss")
  val EventRegex = s"""^(${timestamp}) ([A-Z]+) (.*)""".r


  def toEvent(eventLines: List[String]): Try[Event] = Try {
    eventLines match {
      case EventRegex(ts, level, message) :: tail => {
        Event(df.parse(ts), level, message, if (tail.nonEmpty) toException(tail).toOption else None)
      }
    }
  }

  def toException(lines: List[String]): Try[Error] = Try {
    val st: NStackTrace = StackTraceParser.parse(lines.mkString("\n"))

    //    st.getTrace.getFrames.asScala.map(f => f.toString).foreach(println)

    Error(st.getTrace.getException.getClassName, st.getTrace.getException.getMessage)
  }

  def extractContainerId(containerHeaderLines: List[String]): String = {
    containerHeaderLines match {
      case ContainerRegex(containerId) :: _ => containerId
    }
  }
}
