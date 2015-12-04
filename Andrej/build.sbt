scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "org.parboiled" %% "parboiled" % "2.1.0",
  "org.scalaz" %% "scalaz-core" % "7.1.4",
  "org.slf4j" % "slf4j-log4j12" % "1.2"
)

scalariformSettings

scalacOptions ++= Seq("-feature")