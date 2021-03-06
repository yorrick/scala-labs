package yorrick.yarnlogs

import org.scalatest.{FlatSpec, Matchers, OptionValues, TryValues}
import scala.io.Source
import YarnParser._
import CollectionImplicits._
import scala.util.Try


class CollectionImplicitsSpec extends FlatSpec with Matchers with OptionValues with TryValues {
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
      "container_1475868393807_0001_01_000017" -> 6
    )
  }

  "YarnParser" should "split container level logs into events" in {
    val s = """16/10/07 20:17:11 INFO EsRDDWriter: Writing to [all_until_2016_39_14_es/article]
        |16/10/07 20:17:11 WARN RestRepository: Cannot find node with id [rH5GOttFRoOzgE60LcGX2Q] (is HTTP enabled?)
        |16/10/07 20:17:11 ERROR Executor: Exception in task 148.0 in stage 12.0 (TID 63145)
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
        |16/10/07 20:17:11 WARN RestRepository: Cannot find node with id [oE68V-QqQMK3FKc6IX-2Sg] (is HTTP enabled?)
        |16/10/07 20:17:11 ERROR Executor: Exception in task 64.0 in stage 12.0 (TID 63061)
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
        |16/10/07 20:17:11 INFO CoarseGrainedExecutorBackend: Got assigned task 63258
        |16/10/07 20:17:11 INFO Executor: Running task 285.0 in stage 12.0 (TID 63258)
        |
      """.stripMargin

    val logEvents = splitContainerLogs(s.split("\n").toList)

    logEvents.size shouldBe 7
  }

  "YarnParser" should "parse simple log event with ERROR" in {
    toEvent(List("16/10/07 20:17:11 INFO EsRDDWriter: Writing to [all_until_2016_39_14_es/article]")) shouldBe LogEvent(
      df.parse("16/10/07 20:17:11"), "INFO", "EsRDDWriter: Writing to [all_until_2016_39_14_es/article]", None
    )
  }

  "YarnParser" should "parse log event with random string" in {
    toEvent(List("toto")) shouldBe DefaultEvent("toto")
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

    toEvent(lines) shouldBe LogEvent(
      df.parse("16/10/07 20:17:11"),
      "ERROR",
      "Executor: Exception in task 148.0 in stage 12.0 (TID 63145)",
      Some(Error(
        "org.elasticsearch.hadoop.EsHadoopIllegalStateException", "Cluster state volatile; cannot find node backing shards - please check whether your cluster is stable"))
    )
  }

  "YarnParser" should "parse exception log event with Exception in message" in {
    val lines = """16/10/07 19:45:51 WARN NettyRpcEndpointRef: Error sending message [message = Heartbeat(16,[Lscala.Tuple2;@8548482,BlockManagerId(16, ip-10-101-183-27.ec2.internal, 45045))] in 1 attempts
      |org.apache.spark.SparkException: Exception thrown in awaitResult
      |	at org.apache.spark.rpc.RpcTimeout$$anonfun$1.applyOrElse(RpcTimeout.scala:77)
      |	at org.apache.spark.rpc.RpcTimeout$$anonfun$1.applyOrElse(RpcTimeout.scala:75)
      |	at scala.runtime.AbstractPartialFunction.apply(AbstractPartialFunction.scala:36)
      |	at org.apache.spark.rpc.RpcTimeout$$anonfun$addMessageIfTimeout$1.applyOrElse(RpcTimeout.scala:59)
      |	at org.apache.spark.rpc.RpcTimeout$$anonfun$addMessageIfTimeout$1.applyOrElse(RpcTimeout.scala:59)
      |	at scala.PartialFunction$OrElse.apply(PartialFunction.scala:167)
      |	at org.apache.spark.rpc.RpcTimeout.awaitResult(RpcTimeout.scala:83)
      |	at org.apache.spark.rpc.RpcEndpointRef.askWithRetry(RpcEndpointRef.scala:102)
      |	at org.apache.spark.executor.Executor.org$apache$spark$executor$Executor$$reportHeartBeat(Executor.scala:518)
      |	at org.apache.spark.executor.Executor$$anon$1$$anonfun$run$1.apply$mcV$sp(Executor.scala:547)
      |	at org.apache.spark.executor.Executor$$anon$1$$anonfun$run$1.apply(Executor.scala:547)
      |	at org.apache.spark.executor.Executor$$anon$1$$anonfun$run$1.apply(Executor.scala:547)
      |	at org.apache.spark.util.Utils$.logUncaughtExceptions(Utils.scala:1857)
      |	at org.apache.spark.executor.Executor$$anon$1.run(Executor.scala:547)
      |	at java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)
      |	at java.util.concurrent.FutureTask.runAndReset(FutureTask.java:308)
      |	at java.util.concurrent.ScheduledThreadPoolExecutor$ScheduledFutureTask.access$301(ScheduledThreadPoolExecutor.java:180)
      |	at java.util.concurrent.ScheduledThreadPoolExecutor$ScheduledFutureTask.run(ScheduledThreadPoolExecutor.java:294)
      |	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142)
      |	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617)
      |	at java.lang.Thread.run(Thread.java:745)
      |Caused by: java.util.ConcurrentModificationException
      |	at java.util.ArrayList.writeObject(ArrayList.java:766)
      |	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
      |	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
      |	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
      |	at java.lang.reflect.Method.invoke(Method.java:498)
      |	at java.io.ObjectStreamClass.invokeWriteObject(ObjectStreamClass.java:1028)
      |	at java.io.ObjectOutputStream.writeSerialData(ObjectOutputStream.java:1496)
      |	at java.io.ObjectOutputStream.writeOrdinaryObject(ObjectOutputStream.java:1432)
      |	at java.io.ObjectOutputStream.writeObject0(ObjectOutputStream.java:1178)
      |	at java.io.ObjectOutputStream.defaultWriteFields(ObjectOutputStream.java:1548)
      |	at java.io.ObjectOutputStream.writeSerialData(ObjectOutputStream.java:1509)
      |	at java.io.ObjectOutputStream.writeOrdinaryObject(ObjectOutputStream.java:1432)
      |	at java.io.ObjectOutputStream.writeObject0(ObjectOutputStream.java:1178)
      |	at java.io.ObjectOutputStream.writeArray(ObjectOutputStream.java:1378)
      |	at java.io.ObjectOutputStream.writeObject0(ObjectOutputStream.java:1174)
      |	at java.io.ObjectOutputStream.defaultWriteFields(ObjectOutputStream.java:1548)
      |	at java.io.ObjectOutputStream.writeSerialData(ObjectOutputStream.java:1509)
      |	at java.io.ObjectOutputStream.writeOrdinaryObject(ObjectOutputStream.java:1432)
      |	at java.io.ObjectOutputStream.writeObject0(ObjectOutputStream.java:1178)
      |	at java.io.ObjectOutputStream.defaultWriteFields(ObjectOutputStream.java:1548)
      |	at java.io.ObjectOutputStream.writeSerialData(ObjectOutputStream.java:1509)
      |	at java.io.ObjectOutputStream.writeOrdinaryObject(ObjectOutputStream.java:1432)
      |	at java.io.ObjectOutputStream.writeObject0(ObjectOutputStream.java:1178)
      |	at java.io.ObjectOutputStream.writeArray(ObjectOutputStream.java:1378)
      |	at java.io.ObjectOutputStream.writeObject0(ObjectOutputStream.java:1174)
      |	at java.io.ObjectOutputStream.defaultWriteFields(ObjectOutputStream.java:1548)
      |	at java.io.ObjectOutputStream.writeSerialData(ObjectOutputStream.java:1509)
      |	at java.io.ObjectOutputStream.writeOrdinaryObject(ObjectOutputStream.java:1432)
      |	at java.io.ObjectOutputStream.writeObject0(ObjectOutputStream.java:1178)
      |	at java.io.ObjectOutputStream.defaultWriteFields(ObjectOutputStream.java:1548)
      |	at java.io.ObjectOutputStream.writeSerialData(ObjectOutputStream.java:1509)
      |	at java.io.ObjectOutputStream.writeOrdinaryObject(ObjectOutputStream.java:1432)
      |	at java.io.ObjectOutputStream.writeObject0(ObjectOutputStream.java:1178)
      |	at java.io.ObjectOutputStream.writeObject(ObjectOutputStream.java:348)
      |	at org.apache.spark.serializer.JavaSerializationStream.writeObject(JavaSerializer.scala:43)
      |	at org.apache.spark.serializer.JavaSerializerInstance.serialize(JavaSerializer.scala:100)
      |	at org.apache.spark.rpc.netty.NettyRpcEnv.serialize(NettyRpcEnv.scala:253)
      |	at org.apache.spark.rpc.netty.NettyRpcEnv.ask(NettyRpcEnv.scala:227)
      |	at org.apache.spark.rpc.netty.NettyRpcEndpointRef.ask(NettyRpcEnv.scala:508)
      |	at org.apache.spark.rpc.RpcEndpointRef.askWithRetry(RpcEndpointRef.scala:101)
      |	... 13 more
     """.stripMargin.split("\n").toList

    toEvent(lines) shouldBe LogEvent(
      df.parse("16/10/07 19:45:51"),
      "WARN",
      "NettyRpcEndpointRef: Error sending message [message = Heartbeat(16,[Lscala.Tuple2;@8548482,BlockManagerId(16, ip-10-101-183-27.ec2.internal, 45045))] in 1 attempts",
      Some(Error(
        // here the parser we use does not work so well
        "org.apache.spark.SparkException", "Exception thrown in awaitResult"))
    )
  }

  "YarnParser" should "analyse small real yarn logs" in {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/ip-10-101-183-27.ec2.internal_8041-small"), "ISO-8859-1").mkString.split("\n").toList

    val containerLogs = splitInstanceLogs(lines)
    containerLogs.size shouldBe 1

    val events: Map[String, List[Event]] = containerLogs
      .mapValues(splitContainerLogs)
      .mapValues(_.map(toEvent))

    events.mapValues(_.size) shouldBe Map("container_1475868393807_0001_01_000063" -> 8)

    events.values.flatten.toList.collect {
      case LogEvent(_, _, _, Some(e)) => e
    }.distinct shouldBe List(Error("org.apache.spark.SparkException", "Exception thrown in awaitResult"))
  }

  "YarnParser" should "analyse real yarn logs" in {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/ip-10-101-183-27.ec2.internal_8041"), "ISO-8859-1").mkString.split("\n").toList

    val containerLogs = splitInstanceLogs(lines)
    containerLogs.size shouldBe 4

    val events: Map[String, List[Event]] = containerLogs
      .mapValues(splitContainerLogs)
      .mapValues(_.map(toEvent))

    events.mapValues(_.size) shouldBe Map(
      "container_1475868393807_0001_01_000063" -> 4365,
      "container_1475868393807_0001_01_000017" -> 4316,
      "container_1475868393807_0001_01_000086" -> 4248,
      "container_1475868393807_0001_01_000040" -> 4176)

    events("container_1475868393807_0001_01_000063").collect {
      case LogEvent(_, _, _, Some(e)) => e
    }.distinct shouldBe List(Error("org.elasticsearch.hadoop.EsHadoopIllegalStateException", "Cluster state volatile; cannot find node backing shards - please check whether your cluster is stable"))
  }
}
