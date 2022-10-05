lazy val root = project
  .in(file("."))
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin)
  .settings(
    name := "ProblemUtils",
    console / initialCommands := "import problemutils.*, extensions.*",
    scalaVersion := "3.2.0",

    version := "0.1.1-SNAPSHOT",
    organization := "com.github.daghemberg",

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.8.0"
    ),    
    
    SiteScaladoc / siteSubdirName := "api/latest",
    git.remoteRepo := "git@github.com:DagHemberg/ProblemUtils.git",
  )
