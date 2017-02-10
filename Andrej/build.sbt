scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalaz" %% "scalaz-core" % "7.1.4",
  "org.slf4j" % "slf4j-log4j12" % "1.2",
  "org.apache.commons" % "commons-lang3" % "3.5"
)

scalariformSettings

scalacOptions ++= Seq("-feature", "-deprecation")