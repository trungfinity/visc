lazy val buildSettings = Seq(
  organization := "com.ngthanhtrung",
  version := "0.0.1",
  scalaVersion := "2.11.8"
)

lazy val deps = Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

lazy val visc = (project in file("."))
  .settings(buildSettings: _*)
  .settings(
    libraryDependencies ++= deps
  )
