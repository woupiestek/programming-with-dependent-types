scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalactic" %% "scalactic" % "3.0.8",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalaz" %% "scalaz-core" % "7.2.28",
  "org.slf4j" % "slf4j-log4j12" % "1.7.28",
  "org.apache.commons" % "commons-text" % "1.7",
  "org.ow2.asm" % "asm" % "7.2"
)
