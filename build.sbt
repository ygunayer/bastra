lazy val akkaVersion = "2.5.0"

enablePlugins(JavaAppPackaging)

lazy val commonSettings = Seq(
    organization := "com.yalingunayer",
    version := "0.0.1",
    scalaVersion := "2.11.6"
)

lazy val deps = Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

lazy val commons = (project in file("bastra-commons")).settings(commonSettings, libraryDependencies ++= deps)
lazy val client = (project in file("bastra-client")).settings(commonSettings, libraryDependencies ++= deps).dependsOn(commons)
lazy val server = (project in file("bastra-server")).settings(commonSettings, libraryDependencies ++= deps).dependsOn(commons)
