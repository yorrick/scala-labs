package yorrick.yarnlogs

import java.text.SimpleDateFormat
import com.jmolly.stacktraceparser.{NStackTrace, StackTraceParser}
import scala.util.Try
import collection.JavaConverters._
import Implicits._
import CollectionImplicits._


object YarnParser {
  def splitInstanceLogs(instanceLogs: List[String]): Map[String, List[String]] = {
    val groups: List[List[String]] = instanceLogs
      .groupPrefix(grep(containerSeparator))
      // drop first and last groups that are binary header and footers
      .drop(1).dropRight(1)

    // removes last line of last group since it's always yarn's binary info
    val lastGroup: List[String] = groups.last.dropRight(1)
    val curratedGroups: List[List[String]] = groups.dropRight(1) ++ List(lastGroup)

    curratedGroups.map { fullContainerLogs =>
      val containerHeader = fullContainerLogs.head
      val containerLogs = fullContainerLogs.tail

      extractContainerId(containerHeader) -> containerLogs
    }.toMap
  }

  def splitContainerLogs(containerLogs: List[String]): List[List[String]] = containerLogs
    .groupPrefix(grep(
      log4jLogEventSeparator,
      timSortFailLogEventSeparator,
      sl4jLogEventSeparator,
      yarnBinaryEventSeparator
    ))

  def toEvent(eventLines: List[String]): Event = {
    eventLines match {
      case LogEventRegex(ts, level, message) :: tail => {
//        if (tail.nonEmpty && toException(tail).isFailure) println(s"==============================<${eventLines.head}>\n<${tail.head}>")

        LogEvent(df.parse(ts), level, message, if (tail.nonEmpty) toException(tail).toOption else None)
      }
      case xs => DefaultEvent(xs.mkString("\n"))
    }
  }

  private def grep(regexes: String*): String => Boolean = line => regexes.exists(regex => line.matches(regex))
  private val container = """container_\d{13}_\d{4}_\d{2}_\d{6}"""
  private val containerSeparator = s"$container.*".i
  private val ContainerRegex = s""".*($container).*""".r

  private val timestamp = """\d{2}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}"""

  private val log4jLogEventSeparator = s"""^${timestamp} .*"""
  private val timSortFailLogEventSeparator = s""".*FAIL IN TIM SORT.*"""
  private val sl4jLogEventSeparator = s"""^SLF4J:.*"""
  private val yarnBinaryEventSeparator = s"""    """

  val df = new SimpleDateFormat("yy/MM/dd hh:mm:ss")
  private val LogEventRegex = s"""^(${timestamp}) ([A-Z]+) (.*)""".r

  private val ExceptionRegex = s"""^(.+): (.+)""".r

  private def extractContainerId(containerHeader: String): String = containerHeader match {
    case ContainerRegex(containerId) => containerId
  }

  // TODO use a parser combinator like scala's default or http://www.lihaoyi.com/fastparse/ to extract more info
  private def toException(lines: List[String]): Try[Error] = Try {
    lines.head match {
      case ExceptionRegex(className, message) => Error(className, message)
    }
//    println("=============================================")
//    println(lines.mkString("\n"))
//    println("=====================")

//    val st: NStackTrace = StackTraceParser.parse(lines.mkString("\n"))
    //    st.getTrace.getFrames.asScala.map(f => f.toString).foreach(println)
//    println("=====================")
//    Error(st.getTrace.getException.getClassName, st.getTrace.getException.getMessage)
  }

}
