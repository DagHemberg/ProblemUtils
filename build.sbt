version := "0.1.0"
versionScheme := Some("early-semver")

lazy val root = project
  .in(file("."))
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin)
  .settings(
    name := "ProblemUtils",
    console / initialCommands := "import problemutils.*, extensions.*",
    scalaVersion := "3.2.0",

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.8.0"
    ),    
    
    SiteScaladoc / siteSubdirName := "api/latest",
    git.remoteRepo := "git@github.com:DagHemberg/ProblemUtils.git",
  )
