ThisBuild / version := "0.2.0"
versionScheme := Some("early-semver")

lazy val root = project
  .in(file("."))
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin)
  .settings(
    name := "ProblemUtils",
    console / initialCommands := "import problemutils.*, extensions.*",
    scalaVersion := "3.3.0",
    scalacOptions ++= Seq("-deprecation", "-feature"),

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.9.1"
    ),    
    
    SiteScaladoc / siteSubdirName := "api/latest",
    git.remoteRepo := "git@github.com:DagHemberg/ProblemUtils.git",
  )
