import sbt._
import Keys._

import pl.project13.scala.sbt.JmhPlugin

object ScatoBuild extends Build {
  val testDeps = Seq("org.scalacheck" %% "scalacheck" % "1.12.5" % "test")

  def module(prjName: String) = Project(
    id = prjName,
    base = file(prjName)).settings(
    name := s"scato-$prjName",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-feature","-deprecation", "-Xlint", "-language:higherKinds"),
    libraryDependencies ++= testDeps ++ Seq(
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
    )
  )

  lazy val root = Project(
    id = "root",
    base = file(".")
  ).aggregate ( typeclass
              , baze
              , free
              , profunctors
              , transformers
              , prelude
              , benchmarks
              , examples )

  lazy val typeclass    = module("typeclass")

  lazy val baze         = module("base").dependsOn(typeclass)

  lazy val free         = module("free").dependsOn(baze)
  lazy val profunctors  = module("profunctors").dependsOn(baze)
  lazy val transformers = module("transformers").dependsOn(baze)

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
        Seq ( "org.scalaz" %% "scalaz-core" % "7.2.0"
            , "org.spire-math" %% "cats" % "0.3.0" )
    )

  lazy val examples     = module("examples").dependsOn( baze
                                                      , profunctors
                                                      , transformers
                                                      , prelude)
}
