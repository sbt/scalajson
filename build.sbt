name := "scalajson"

import PgpKeys.publishSigned

// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.crossProject

val currentScalaVersion = "2.13.0"
val scala210Version = "2.10.7"
val scala212Version = "2.12.8"
val scala213Version = "2.13.0"
val scalaCheckVersion = Def.setting(CrossVersion partialVersion scalaVersion.value match {
  case Some((2, 10)) => "1.13.5"
  case _             => "1.14.0"
})
val specs2Version = Def.setting(CrossVersion partialVersion scalaVersion.value match {
  case Some((2, 10)) => "3.9.5"
  case _             => "4.6.0"
})
val utestVersion = Def.setting(CrossVersion partialVersion scalaVersion.value match {
  case Some((2, 10)) => "0.6.8"
  case Some((2, 11)) => "0.6.8"
  case _             => "0.7.1"
})

// WORKAROUND https://github.com/sbt/sbt/issues/3353
val scalaVersionSettings = Def settings (
  scalaVersion := currentScalaVersion,
  crossScalaVersions := Seq(currentScalaVersion, scala212Version, scala213Version, scala210Version)
)
inThisBuild(scalaVersionSettings)
scalaVersionSettings

autoAPIMappings := true

val flagsFor10 = Seq(
  "-Xlint",
  "-Yclosure-elim",
  "-Ydead-code"
)

val flagsFor11 = Seq(
  "-Xlint:_",
  "-Yconst-opt",
  "-Ywarn-infer-any",
  "-Yclosure-elim",
  "-Ydead-code",
  "-Xsource:2.12" // required to build case class construction
)

lazy val root = project
  .in(file("."))
  .aggregate(scalaJsonJS, scalaJsonJVM)
  .settings(
    publish := {},
    publishLocal := {},
    publishSigned := {}
  )

lazy val commonSettings = Def settings (
  name := "shaded-scalajson",
  version := "1.0.0-M4",
  organization := "com.eed3si9n",
  scalaVersionSettings,
  scalacOptions ++= Seq(
    "-encoding",
    "UTF-8",
    "-deprecation", // warning and location for usages of deprecated APIs
    "-feature", // warning and location for usages of features that should be imported explicitly
    "-unchecked", // additional warnings where generated code depends on assumptions
    "-Xlint", // recommended additional warnings
    "-Xcheckinit", // runtime error when a val is not initialized due to trait hierarchies (instead of NPE somewhere else)
    "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
    "-Ywarn-dead-code"
  ),
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  pomIncludeRepository := (_ => false),
  homepage := Some(url("https://github.com/mdedetrich/scalajson")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/mdedetrich/scalajson"),
            "git@github.com:mdedetrich/scalajson.git")),
  developers := List(
    Developer("mdedetrich",
              "Matthew de Detrich",
              "mdedetrich@gmail.com",
              url("https://github.com/mdedetrich"))
  ),
  licenses += ("BSD 3 Clause", url(
    "https://opensource.org/licenses/BSD-3-Clause"))
)

lazy val scalaJson = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    commonSettings,
    // In our build, implementations are specific due to use using sealed traits so a build defined
    // in scala-2.10 can't use the same sources as the generic 'scala' build. This removes the 'scala'
    // directory from sources when building for Scala 2.10.x
    (unmanagedSourceDirectories in Compile) := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 11 =>
          (unmanagedSourceDirectories in Compile).value
        case Some((2, n)) if n == 10 =>
          (unmanagedSourceDirectories in Compile).value.filter { x =>
            !x.getName.endsWith("scala")
          }
      }
    },
    scalacOptions += {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 12 =>
          "-target:jvm-1.8"
        case _ =>
          "-target:jvm-1.6"
      }
    },
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 12 => Nil
        case Some((2, n)) if n == 11 => flagsFor11
        case Some((2, n)) if n == 10 => flagsFor10
      }
    }
  )
  .jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % specs2Version.value % Test,
      "org.specs2" %% "specs2-scalacheck" % specs2Version.value % Test,
      "org.scalacheck" %% "scalacheck" % scalaCheckVersion.value % Test
    ),
    scalacOptions in Test ++= Seq("-Yrangepos"),
    mimaPreviousArtifacts := (CrossVersion partialVersion scalaVersion.value match {
      case Some((2, n)) if n >= 13 => Set.empty
      case _                       => Set("com.eed3si9n" %% "shaded-scalajson" % "1.0.0-M4")
    })
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion.value % Test,
      "com.lihaoyi" %%% "utest" % utestVersion.value % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val benchmark = crossProject(JSPlatform, JVMPlatform)
  .in(file("benchmark"))
  .jvmSettings(
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.7" % Test
    )
  )
  .dependsOn(scalaJson)

lazy val scalaJsonJVMTest = benchmark.jvm
lazy val scalaJsonJSTest = benchmark.js

lazy val scalaJsonJVM = scalaJson.jvm
lazy val scalaJsonJS = scalaJson.js
