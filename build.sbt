//sbtavrohugger.SbtAvrohugger.avroSettings

sbtavrohugger.SbtAvrohugger.specificAvroSettings

name := "algorithms1"

organization := "yorrick"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
    "org.scalatest"   %% "scalatest"    % "2.2.4"   % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
    "junit" % "junit" % "4.12" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test",
    "com.gensler" %% "scalavro" % "0.6.2",
    "org.scaldi" %% "scaldi" % "0.3.2",
    "net.codingwell" %% "scala-guice" % "4.0.1",
    "com.softwaremill.macwire" %% "macros" % "1.0.7",
    "com.softwaremill.macwire" %% "runtime" % "1.0.7"
)

scalacOptions ++= List("-feature","-deprecation", "-unchecked", "-Xlint")

testOptions in Test += Tests.Argument("-oDF")
