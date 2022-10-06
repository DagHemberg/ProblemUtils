import xerial.sbt.Sonatype._

description := "A collection of utilities made to be useful for solving various coding problems, such as Advent of Code or Project Euler."

organization := "io.github.daghemberg"
sonatypeProfileName := "io.github.daghemberg"
sonatypeCredentialHost := "s01.oss.sonatype.org"
licenses := List(librarymanagement.License.MIT)
sonatypeProjectHosting := Some(GitHubHosting("DagHemberg", "problemutils", "dag.hemberg@gmail.com"))
pomIncludeRepository := { _ => false }

publishTo := sonatypePublishToBundle.value
publishMavenStyle := true

developers := List(
  librarymanagement.Developer(
    "daghemberg", 
    "Dag Hemberg", 
    "dag.hemberg@gmail.com", 
    url("https://github.com/daghemberg/")
  )
)