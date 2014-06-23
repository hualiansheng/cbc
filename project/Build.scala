import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Seq(
    scalaVersion := "2.11.1",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.1.7" % "test",
      "com.chuusai" %% "shapeless" % "2.0.0"
    ),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases")
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val main = Project(
    "main", 
    file("."), 
    settings = buildSettings ++ Seq(
      includeFilter in (Compile, unmanagedSources) := "SafeTrees.scala",
      includeFilter in (Test, unmanagedSources) := "Test.scala"
    )
  ) dependsOn(macroSub, commonSub)
  
  lazy val macroSub = Project(
    "macro", 
    file("macro"),
    settings = buildSettings ++ Seq(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  ) dependsOn(commonSub)

  lazy val commonSub = Project(
    "common",
    file("common"),
    settings = buildSettings
  )

}
