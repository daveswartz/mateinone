name := "mateinone"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "2.3.6" % "test")

scalacOptions in Compile ++= Seq("-unchecked", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

initialCommands in console :=
  """
    |import mateinone._
    |import GithubFlavoredMarkdownPrinter._
    |import Square._
    |import MoveImplicits._
  """.stripMargin
