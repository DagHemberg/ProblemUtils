val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin)
  .settings(
    console / initialCommands := "import problemutils.*, extensions.*",
    name := "ProblemUtils",
    scalaVersion := scala3Version,
    SiteScaladoc / siteSubdirName := "api/latest",
    git.remoteRepo := "git@github.com:DagHemberg/ProblemUtils.git",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.8.0"
    ),
  )
