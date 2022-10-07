import xerial.sbt.Sonatype._

lazy val id = "DagHemberg"
lazy val org = "io.github.daghemberg"
lazy val mail = "dag.hemberg@gmail.com"

description := "A collection of utilities made to be useful for solving various coding problems, such as Advent of Code or Project Euler."

organization := org
sonatypeProfileName := org
sonatypeCredentialHost := "s01.oss.sonatype.org"
licenses := List(librarymanagement.License.MIT)
sonatypeProjectHosting := Some(GitHubHosting(id, "problemutils", mail))
pomIncludeRepository := { _ => false }

publishTo := sonatypePublishToBundle.value
publishMavenStyle := true

developers := List(
  librarymanagement.Developer(id, "Dag Hemberg", mail, url("https://github.com/daghemberg/"))
)