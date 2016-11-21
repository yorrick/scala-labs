package yorrick.yarnlogs

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import scala.io.Source
import YarnParser._
import CollectionImplicits._


class CollectionImplicitsSpec extends FlatSpec with Matchers with OptionValues with TryValues {
  //  container_1475868393807_0001_01_000063
  //  container_1475868393807_0001_01_000017
  //  container_1475868393807_0001_01_000086 8905
  //  container_1475868393807_0001_01_000040 13299
  //  container_1475868393807_0001_01_000063 17542

  "groupPrefix" should "group strings by prefix when prefixes are present" in {
    val s =
      """toto
        |---
        |tata
        |titi
        |---
        |tutu
        |---""".stripMargin


    val groups: List[List[String]] = s.split("\n").toList.groupPrefix(_ == "---")
    groups.size shouldBe 4

    groups shouldBe List(
      List("toto"),
      List("---", "tata", "titi"),
      List("---", "tutu"),
      List("---")
    )

    groups.flatten shouldBe s.split("\n")
  }

  "groupPrefix" should "group strings by prefix when prefixes are not there" in {
    val s =
      """toto
        |---
        |tata
        |titi
        |---
        |tutu
        |---""".stripMargin


    val groups: List[List[String]] = s.split("\n").toList.groupPrefix(_ == "========")
    groups.size shouldBe 1

    groups shouldBe List(List("toto", "---", "tata", "titi", "---", "tutu", "---"))

    groups.flatten shouldBe s.split("\n")
  }

  "groupPrefix" should "handle empty strings" in {
    val groups: List[List[String]] = List.empty[String].groupPrefix(_ == "---")
    groups.size shouldBe 0
  }
}


class LogParsingSpec extends FlatSpec with Matchers with OptionValues with TryValues {
  "YarnParser" should "split instance level logs into container level logs" in {
    val s = """—”hëµ◊∂9ﬂA@í∫·P	 VERSION    APPLICATION_ACL2 
        |MODIFY_APP^@^Lyarn,hadoop ^S^@^QAPPLICATION_OWNER^H^@^Fhadoop(^@&container_1475868393807_0001_01_000063¯Ëg^@^Fstderr^@^F502857SLF4J: Class path contains multiple SLF4J bindings.
        |SLF4J: Found binding in [jar:file:/mnt1/yarn/usercache/hadoop/filecache/14/__spark_libs__7990095660427488796.zip/slf4j-log4j12-1.7.16.jar!/org/slf4j/impl/StaticLoggerBinder.class]
        |SLF4J: Found binding in [jar:file:/usr/lib/hadoop/lib/slf4j-log4j12-1.7.10.jar!/org/slf4j/impl/StaticLoggerBinder.class]
        |SLF4J: See http://www.slf4j.org/codes.html#multiple_bindings for an explanation.
        |SLF4J: Actual binding is of type [org.slf4j.impl.Log4jLoggerFactory]
        |16/10/07 19:35:18 INFO CoarseGrainedExecutorBackend: Started daemon with process name: 9732@ip-10-101-183-27
        |16/10/07 19:35:18 INFO SignalUtils: Registered signal handler for TERM
        |(^@&container_1475868393807_0001_01_000017°^HÂ^@^Fstderr^@^F519680SLF4J: Class path contains multiple SLF4J bindings.
        |SLF4J: Found binding in [jar:file:/mnt1/yarn/usercache/hadoop/filecache/14/__spark_libs__7990095660427488796.zip/slf4j-log4j12-1.7.16.jar!/org/slf4j/impl/StaticLoggerBinder.class]
        |SLF4J: Found binding in [jar:file:/usr/lib/hadoop/lib/slf4j-log4j12-1.7.10.jar!/org/slf4j/impl/StaticLoggerBinder.class]
        |SLF4J: See http://www.slf4j.org/codes.html#multiple_bindings for an explanation.
        |SLF4J: Actual binding is of type [org.slf4j.impl.Log4jLoggerFactory]
        |16/10/07 19:35:18 INFO CoarseGrainedExecutorBackend: Started daemon with process name: 9733@ip-10-101-183-27
        |16/10/07 19:35:18 INFO SignalUtils: Registered signal handler for TERM
        |^@^A^@^@^G^@
        |        ^@^GVERSION*(^@&container_1475868393807_0001_01_000063^D*(^@&container_1475868393807_0001_01_000017^A*(^@&container_1475868393807_0001_01_000086^A*(^@&container_1475868393807_0001_01_000040^A^Dnone^D^P¯Ì^D¯Ì^D¯Ì^T°^Hî°^Hî·      Õ^B¯ª
        |¯ª
        |<90>^W^?^L¯J¤¯J¤^C^Qdata:BCFile.index^Dnone<90>^^Êm))^Pdata:TFile.index^Dnone<90>^^É¶Ì·Ì·^Odata:TFile.meta^Dnone<90>^^É°^F^F^@^@^@^@^@^^Ê<96>^@^A^@^@Ñ^QÓh<91>µ×¶9ßA@<92>ºáP
      """.stripMargin

    val containerLogs = splitInstanceLogs(s.split("\n").toList)
    containerLogs.size shouldBe 2

    containerLogs.mapValues(_.size) shouldBe Map(
      "container_1475868393807_0001_01_000063" -> 6,
      "container_1475868393807_0001_01_000017" -> 7
    )
  }

//  "YarnParser" should "be able to split java exceptions" in {
//    val s = """16/10/07 20:17:11 INFO EsRDDWriter: Writing to [all_until_2016_39_14_es/article]
//              |16/10/07 20:17:11 WARN RestRepository: Cannot find node with id [rH5GOttFRoOzgE60LcGX2Q] (is HTTP enabled?)
//              |16/10/07 20:17:11 ERROR Executor: Exception in task 148.0 in stage 12.0 (TID 63145)
//              |org.elasticsearch.hadoop.EsHadoopIllegalStateException: Cluster state volatile; cannot find node backing shards - please check whether your cluster is stable
//              |	at org.elasticsearch.hadoop.rest.RestRepository.getWriteTargetPrimaryShards(RestRepository.java:314)
//              |	at org.elasticsearch.hadoop.rest.RestService.initSingleIndex(RestService.java:599)
//              |	at org.elasticsearch.hadoop.rest.RestService.createWriter(RestService.java:563)
//              |	at org.elasticsearch.spark.rdd.EsRDDWriter.write(EsRDDWriter.scala:40)
//              |	at org.elasticsearch.spark.rdd.EsSpark$$anonfun$doSaveToEs$1.apply(EsSpark.scala:84)
//              |	at org.elasticsearch.spark.rdd.EsSpark$$anonfun$doSaveToEs$1.apply(EsSpark.scala:84)
//              |	at org.apache.spark.scheduler.ResultTask.runTask(ResultTask.scala:70)
//              |	at org.apache.spark.scheduler.Task.run(Task.scala:85)
//              |	at org.apache.spark.executor.Executor$TaskRunner.run(Executor.scala:274)
//              |	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142)
//              |	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617)
//              |	at java.lang.Thread.run(Thread.java:745)
//              |16/10/07 20:17:11 WARN RestRepository: Cannot find node with id [oE68V-QqQMK3FKc6IX-2Sg] (is HTTP enabled?)
//              |16/10/07 20:17:11 ERROR Executor: Exception in task 64.0 in stage 12.0 (TID 63061)
//              |org.elasticsearch.hadoop.EsHadoopIllegalStateException: Cluster state volatile; cannot find node backing shards - please check whether your cluster is stable
//              |	at org.elasticsearch.hadoop.rest.RestRepository.getWriteTargetPrimaryShards(RestRepository.java:314)
//              |	at org.elasticsearch.hadoop.rest.RestService.initSingleIndex(RestService.java:599)
//              |	at org.elasticsearch.hadoop.rest.RestService.createWriter(RestService.java:563)
//              |	at org.elasticsearch.spark.rdd.EsRDDWriter.write(EsRDDWriter.scala:40)
//              |	at org.elasticsearch.spark.rdd.EsSpark$$anonfun$doSaveToEs$1.apply(EsSpark.scala:84)
//              |	at org.elasticsearch.spark.rdd.EsSpark$$anonfun$doSaveToEs$1.apply(EsSpark.scala:84)
//              |	at org.apache.spark.scheduler.ResultTask.runTask(ResultTask.scala:70)
//              |	at org.apache.spark.scheduler.Task.run(Task.scala:85)
//              |	at org.apache.spark.executor.Executor$TaskRunner.run(Executor.scala:274)
//              |	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142)
//              |	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617)
//              |	at java.lang.Thread.run(Thread.java:745)
//              |16/10/07 20:17:11 INFO CoarseGrainedExecutorBackend: Got assigned task 63258
//              |16/10/07 20:17:11 INFO Executor: Running task 285.0 in stage 12.0 (TID 63258)
//              |
//            """.stripMargin
//
//    val logEvents = s.split("\n").toList.groupPrefix(grep(logEventSeparator))
//
//    logEvents.size shouldBe 7
//  }
//
//  "YarnParser" should "be able to split analyse a real yarn log" in {
//    val lines = Source.fromInputStream(getClass.getResourceAsStream("/ip-10-101-183-27.ec2.internal_8041"), "ISO-8859-1").mkString.split("\n").toList
//
//    // removes log file header and footer
//    val containerLogs = lines.groupPrefix(grep(containerSeparator)).drop(1).dropRight(1)
//    containerLogs.size shouldBe 4
//
//    val logContent = containerLogs.map(logs => logs.groupPrefix(grep(logEventSeparator))).map { events: List[List[String]] =>
//      extractContainerId(events.head) -> events.tail
//
////      println("======================")
////      println(contain)
////      println(s"First event lines: ${events(0).size}")
////      events(0).foreach(println)
//
////      println(s"Number of events: ${events.size}")
//    }
//
//    logContent.map { case (container, events) => (container, events.size) } shouldBe List(
//      ("container_1475868393807_0001_01_000063", 4105),
//      ("container_1475868393807_0001_01_000017", 4092),
//      ("container_1475868393807_0001_01_000086", 3988),
//      ("container_1475868393807_0001_01_000040", 3945)
//    )
//
////    logContent.map { case (container, events) => (container, events.map(toEvent)) }
//  }

  "YarnParser" should "parse simple log event" in {
    toEvent(List("16/10/07 20:17:11 INFO EsRDDWriter: Writing to [all_until_2016_39_14_es/article]")).success.value shouldBe Event(
      df.parse("16/10/07 20:17:11"), "INFO", "EsRDDWriter: Writing to [all_until_2016_39_14_es/article]", None
    )
  }

  "YarnParser" should "parse exception log event" in {
    val lines = """16/10/07 20:17:11 ERROR Executor: Exception in task 148.0 in stage 12.0 (TID 63145)
       |org.elasticsearch.hadoop.EsHadoopIllegalStateException: Cluster state volatile; cannot find node backing shards - please check whether your cluster is stable
       |	at org.elasticsearch.hadoop.rest.RestRepository.getWriteTargetPrimaryShards(RestRepository.java:314)
       |	at org.elasticsearch.hadoop.rest.RestService.initSingleIndex(RestService.java:599)
       |	at org.elasticsearch.hadoop.rest.RestService.createWriter(RestService.java:563)
       |	at org.elasticsearch.spark.rdd.EsRDDWriter.write(EsRDDWriter.scala:40)
       |	at org.elasticsearch.spark.rdd.EsSpark$$anonfun$doSaveToEs$1.apply(EsSpark.scala:84)
       |	at org.elasticsearch.spark.rdd.EsSpark$$anonfun$doSaveToEs$1.apply(EsSpark.scala:84)
       |	at org.apache.spark.scheduler.ResultTask.runTask(ResultTask.scala:70)
       |	at org.apache.spark.scheduler.Task.run(Task.scala:85)
       |	at org.apache.spark.executor.Executor$TaskRunner.run(Executor.scala:274)
       |	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142)
       |	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617)
       |	at java.lang.Thread.run(Thread.java:745)
     """.stripMargin.split("\n").toList

    toEvent(lines).success.value shouldBe Event(
      df.parse("16/10/07 20:17:11"),
      "ERROR",
      "Executor: Exception in task 148.0 in stage 12.0 (TID 63145)",
      Some(Error(
        "org.elasticsearch.hadoop.EsHadoopIllegalStateException", ": Cluster state volatile; cannot find node backing shards - please check whether your cluster is stable"))
    )
  }
}
