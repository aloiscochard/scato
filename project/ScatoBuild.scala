import sbt._
import Keys._

import pl.project13.scala.sbt.JmhPlugin

object ScatoBuild extends Build {
  val testDeps = Seq("org.scalacheck" %% "scalacheck" % "1.13.0" % "test")

  def module(prjName: String) = Project(
    id = prjName,
    base = file(prjName)).settings(
    name := s"scato-$prjName",
    scalaVersion := "2.11.8",
    scalacOptions ++= Seq("-feature","-deprecation", "-Xlint", "-language:higherKinds",
                          "-Ybackend:GenBCode", "-Ydelambdafy:method", "-target:jvm-1.8"),
    libraryDependencies ++= testDeps ++ Seq(
      "org.scala-lang.modules" %% "scala-java8-compat" % "0.7.0",
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
    )
  )

  lazy val root = Project(
    id = "root",
    base = file(".")
  ).aggregate ( baze
              , free
              , profunctors
              , transformers
              , io
              , prelude
              , benchmarks
              , examples )

  lazy val baze         = module("base")

  lazy val free         = module("free").dependsOn(baze)
  lazy val profunctors  = module("profunctors").dependsOn(baze)
  lazy val transformers = module("transformers").dependsOn(baze)

  lazy val io           = module("io").dependsOn(baze)

  lazy val prelude      = module("prelude").dependsOn(baze)

  lazy val benchmarks   = module("benchmarks")
    .dependsOn( baze
              , free
              , profunctors
              , transformers
              , prelude)
    .enablePlugins(JmhPlugin)
    .settings(
      libraryDependencies ++=
        Seq ( "org.scala-lang" % "scala-reflect" % scalaVersion.value
            , "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
            , "org.scalaz" %% "scalaz-core" % "7.2.1"
            , "org.typelevel" %% "cats" % "0.5.0" )
    )

  lazy val examples     = module("examples").dependsOn( baze
                                                      , profunctors
                                                      , transformers
                                                      , io
                                                      , prelude)
}
