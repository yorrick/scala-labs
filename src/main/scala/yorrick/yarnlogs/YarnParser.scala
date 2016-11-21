package yorrick.yarnlogs

import java.text.SimpleDateFormat
import com.jmolly.stacktraceparser.{NStackTrace, StackTraceParser}
import scala.util.Try
import collection.JavaConverters._
import Implicits._
import CollectionImplicits._


object CollectionImplicits {
  implicit class RichList[T](xs: List[T]) {
    /** Returns shortest possible list of lists xss such that
      *   - xss.flatten == xs
      *   - No sublist in xss contains an element matching p in its tail
      */
    def groupPrefix(p: T => Boolean): List[List[T]] = xs match {
      case List() => List()
      case x :: xs1 =>
        val (ys, zs) = xs1 span (!p(_))
        (x :: ys) :: zs.groupPrefix(p)
    }
  }
}


object YarnParser {
  def splitInstanceLogs(instanceLogs: List[String]): Map[String, List[String]] = instanceLogs
    .groupPrefix(grep(containerSeparator))
    // drop first and last groups that are binary header and footers
    .drop(1).dropRight(1)
    .map { fullContainerLogs =>
      val containerHeader = fullContainerLogs.head
      val containerLogs = fullContainerLogs.tail

      extractContainerId(containerHeader) -> containerLogs
    }.toMap

  private def grep(regexes: String*): String => Boolean = line => regexes.exists(regex => line.matches(regex))
  private val container = """container_\d{13}_\d{4}_\d{2}_\d{6}"""
  private val containerSeparator = s"$container.*".i
  private val ContainerRegex = s""".*($container).*""".r

  private val timestamp = """\d{2}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}"""
  private val logEventSeparator = s"""^${timestamp} .*"""

  val df = new SimpleDateFormat("yy/MM/dd hh:mm:ss")
  private val EventRegex = s"""^(${timestamp}) ([A-Z]+) (.*)""".r

  private def extractContainerId(containerHeader: String): String = containerHeader match {
    case ContainerRegex(containerId) => containerId
  }

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

}
