name := "algorithms1"

organization := "yorrick"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
    "org.scalatest"   %% "scalatest"    % "2.2.4"   % "test",
    "junit" % "junit" % "4.12" % "test",
    "com.novocode" % "junit-interface" % "0.11" % "test"
)

scalacOptions ++= List("-feature","-deprecation", "-unchecked", "-Xlint")

testOptions in Test += Tests.Argument("-oDF")
