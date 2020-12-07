name := "taxero"
organization := "org.clulab"

val procVersion = "8.2.3"
val procModelVer = "7.5.4"
val odinsonVer = "0.3.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.clulab" %% "processors-main" % procVersion,
  "org.clulab" %% "processors-corenlp" % procVersion,
  "org.clulab" %% "processors-modelsmain" % procModelVer,
  "org.clulab" %% "processors-modelscorenlp" % procModelVer,
  "ai.lum" %% "odinson-core" % odinsonVer,
  "ai.lum" %% "odinson-extra" % odinsonVer,
  "ai.lum" %% "common" % "0.0.9",
)

lazy val core = project in file(".")

lazy val webapp = project
  .enablePlugins(PlayScala)
  .aggregate(core)
  .dependsOn(core)
