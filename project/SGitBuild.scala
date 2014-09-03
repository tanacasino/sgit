import sbt._
import sbt.Keys._

object SGitBuild extends Build {

  lazy val sgit = Project(
    id = "sgit",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "sgit",
      organization := "com.github.tanacasino",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.11.2",
      // add other settings here
      libraryDependencies ++= Seq(
        "org.eclipse.jgit" % "org.eclipse.jgit" % "3.4.1.201406201815-r",
        "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
      )
    )
  )
}
