//sbtavrohugger.SbtAvrohugger.avroSettings

sbtavrohugger.SbtAvrohugger.specificAvroSettings

name := "algorithms1"

organization := "yorrick"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
    "org.scalatest"   %% "scalatest"    % "2.2.4"   % "test",
    "junit" % "junit" % "4.12" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test",
    "com.gensler" %% "scalavro" % "0.6.2"
)

scalacOptions ++= List("-feature","-deprecation", "-unchecked", "-Xlint")

testOptions in Test += Tests.Argument("-oDF")
