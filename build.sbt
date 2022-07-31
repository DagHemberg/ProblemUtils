val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin)
  .settings(
    name := "ProblemUtils",
    scalaVersion := scala3Version,
    SiteScaladoc / siteSubdirName := "api/latest",
    git.remoteRepo := "git@github.com:DagHemberg/ProblemUtils.git",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.8.0"
    ),
  )
