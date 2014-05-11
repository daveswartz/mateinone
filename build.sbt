name := "mateinone"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.0"

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "2.3.11" % "test")

initialCommands in console :=
  """
    |import mateinone._
    |import TerminalPrinter._
    |import Square._
    |import MoveImplicits._
  """.stripMargin
