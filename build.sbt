import sbt.Keys.{scalaVersion, _}
import sbt.dsl._

val scalazVersion = "7.2.8"
val paradiseVersion = "2.1.0"

lazy val main = Project(
    "scala-labs",
    file("."),
    settings = Seq(
        organization := "yorrick",
        version := "1.0",
        scalaVersion := "2.11.8",
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
            "org.scalaz" %% "scalaz-core" % scalazVersion,
            "org.scalaz" %% "scalaz-effect" % scalazVersion,
        //    "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
            "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
        ),
        scalacOptions ++= List("-feature","-deprecation", "-unchecked", "-Xlint"),
        testOptions in Test += Tests.Argument("-oDF"),
        initialCommands in console := "import scalaz._, Scalaz._",
        initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._",
        addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
    )
) dependsOn (
  macrosSub
) aggregate(
  macrosSub
)

lazy val macrosSub = Project(
    "macros",
    file("macros"),
    settings = Seq(
      scalacOptions ++= List("-feature","-deprecation", "-unchecked", "-Xlint"),
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
)


