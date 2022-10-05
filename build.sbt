val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin)
  .settings(
    name := "ProblemUtils",
    console / initialCommands := "import problemutils.*, extensions.*",
    githubOwner := "DagHemberg",
    githubRepository := "ProblemUtils",
    publishMavenStyle := true,
    scalaVersion := scala3Version,
    SiteScaladoc / siteSubdirName := "api/latest",
    git.remoteRepo := "git@github.com:DagHemberg/ProblemUtils.git",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.8.0"
    ),
  )
