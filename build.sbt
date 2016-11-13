lazy val buildSettings = Seq(
  organization := "com.ngthanhtrung",
  version := "0.0.1",
  scalaVersion := "2.12.0"
)

lazy val deps = Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1"
)

lazy val visc = (project in file("."))
  .settings(buildSettings: _*)
  .settings(
    libraryDependencies ++= deps
  )
