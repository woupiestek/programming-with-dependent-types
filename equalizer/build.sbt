scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.27",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test")

scalacOptions ++= Seq("-feature", "-deprecation")