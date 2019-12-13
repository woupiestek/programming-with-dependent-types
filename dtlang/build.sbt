scalaVersion := "2.12.9"

organization := "nl.woupiestek.dtlang"

val zioVersion = "1.0.0-RC17"
libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % "test",
  "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
)
testFrameworks ++= Seq(
  new TestFramework("zio.test.sbt.ZTestFramework")
)
