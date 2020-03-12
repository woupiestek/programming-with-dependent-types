scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.28",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalactic" %% "scalactic" % "3.0.8",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
