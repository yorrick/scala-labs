//sbtavrohugger.SbtAvrohugger.avroSettings

//sbtavrohugger.SbtAvrohugger.specificAvroSettings

name := "scala-labs"

organization := "yorrick"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
    "org.scalatest"   %% "scalatest"    % "2.2.4"   % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
    "junit" % "junit" % "4.12" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test",
//    "com.gensler" %% "scalavro" % "0.6.2",
    "com.typesafe" % "config" % "1.2.1",
    "org.scaldi" %% "scaldi" % "0.5.7",
    "net.codingwell" %% "scala-guice" % "4.0.1",
    "com.softwaremill.macwire" %% "macros" % "1.0.7",
    "com.softwaremill.macwire" %% "runtime" % "1.0.7",
    "org.antlr" % "antlr-runtime" % "3.4",
    "org.scalaz" %% "scalaz-core" % "7.2.7",
    "org.scalaz" %% "scalaz-effect" % "7.2.7"
//    "org.scalaz" %% "scalaz-typelevel" % "7.2.7",
//    "org.scalaz" %% "scalaz-scalacheck-binding" % "7.2.7" % "test"
)

scalacOptions ++= List("-feature","-deprecation", "-unchecked", "-Xlint")

testOptions in Test += Tests.Argument("-oDF")
