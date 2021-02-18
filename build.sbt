import play.sbt.PlayImport.guice

organization in ThisBuild := "org.clulab"
name := "taxero"

scalaVersion in ThisBuild := "2.12.4"

val procVersion = "8.2.3"
val procModelVer = "7.5.4"
val odinsonVer = "0.3.0-SNAPSHOT"

disablePlugins( PlayScala, SbtNativePackager )

lazy val core = ( project in file( "." ) )
  .disablePlugins( PlayScala, SbtNativePackager )
  .settings(
      libraryDependencies ++= Seq(
          "org.clulab" %% "processors-main" % procVersion,
          "org.clulab" %% "processors-corenlp" % procVersion,
          "org.clulab" %% "processors-modelsmain" % procModelVer,
          "org.clulab" %% "processors-modelscorenlp" % procModelVer,
          "ai.lum" %% "odinson-core" % odinsonVer,
          "ai.lum" %% "odinson-extra" % odinsonVer,
          "ai.lum" %% "common" % "0.0.9" )
      )

lazy val webapp = project
  .enablePlugins(PlayScala, SbtNativePackager)
  .settings(
      libraryDependencies ++= Seq(
          guice,
          "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test )
      )
  .aggregate( core )
  .dependsOn( core )
