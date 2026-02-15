name := "mateinone"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.14"

resolvers ++= Resolver.sonatypeOssRepos("snapshots") ++ Resolver.sonatypeOssRepos("releases")

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.20.4" % "test")

console / initialCommands :=
  """
    |import mateinone._
    |import TerminalPrinter._
    |import Square._
    |import MoveImplicits._
  """.stripMargin
