val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "ProblemUtils",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.8.0"
    ),
  )
