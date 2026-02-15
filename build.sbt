name := "mateinone"

version := "0.1-SNAPSHOT"

// Fixed Scala version for binary compatibility with scoverage
scalaVersion := "2.13.14"

resolvers ++= Resolver.sonatypeOssRepos("snapshots") ++ Resolver.sonatypeOssRepos("releases")

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.20.4" % "test")

// Usage:
// - sbt test: Run all unit tests
// - sbt coverage test coverageReport: Generate code coverage report in target/scala-2.13/scoverage-report
// - sbt "run --depth 8": Run engine simulation
console / initialCommands :=
  """
    |import mateinone.bitboard._
    |import mateinone.TerminalPrinter._
  """.stripMargin
