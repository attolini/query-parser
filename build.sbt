name := "query-parser"

version := "0.0.1"

scalaVersion := "2.13.11"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"

/* LIBRARIES FOR TEST */
libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "3.1.1"  % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.3" % Test
)
