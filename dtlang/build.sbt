scalaVersion := "2.12.8"

organization := "nl.woupiestek.dtlang"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.parboiled" %% "parboiled" % "2.1.8",
  "org.scalaz" %% "scalaz-core" % "7.2.27",
  "org.slf4j" % "slf4j-log4j12" % "1.2"
)
