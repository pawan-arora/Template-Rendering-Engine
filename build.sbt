import com.typesafe.sbt.packager.docker._

name := "Template"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.5"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

val AkkaVersion = "2.6.12"
val AkkaHttpVersion = "10.2.3"
val AkkaManagementVersion = "1.0.9"
val CirceVersion = "0.12.3"

lazy val akkaDependencies = Seq(
  "com.typesafe.akka" %% "akka-serialization-jackson" % AkkaVersion,
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-cluster-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-remote" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % AkkaHttpVersion,
  "com.typesafe.akka" %% "akka-discovery" % AkkaVersion,
  "com.typesafe.akka" %% "akka-persistence-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-cluster-sharding-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-protobuf" % AkkaVersion,
  "com.lightbend.akka.management" %% "akka-management" % AkkaManagementVersion,
  "com.lightbend.akka.management" %% "akka-management-cluster-bootstrap" % AkkaManagementVersion,
  "com.lightbend.akka.management" %% "akka-management-cluster-http" % AkkaManagementVersion,
  "com.typesafe.akka" %% "akka-testkit" % AkkaVersion % Test,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  "org.scalatest"     %% "scalatest" % "3.1.4" % Test
)

lazy val circeDependencies = Seq(
  "io.circe" %% "circe-core" % CirceVersion,
  "io.circe" %% "circe-generic" % CirceVersion,
  "io.circe" %% "circe-parser" % CirceVersion
)

lazy val root = (project in file("."))
  .enablePlugins(DockerPlugin, JavaAppPackaging)
  .settings(
    libraryDependencies ++= akkaDependencies ++
      circeDependencies ++
      Seq(
        "io.github.daviddenton" %% "handlebars-scala-fork" % "2.3.0",
        "ch.qos.logback" % "logback-classic" % "1.2.3",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
        "org.scalactic" %% "scalactic" % "3.2.2" % Test,
        "org.scalatest" %% "scalatest" % "3.2.2" % Test,
        "org.mockito" %% "mockito-scala" % "1.16.15" % Test,
        "commons-io" % "commons-io" % "2.6",
        "commons-codec" % "commons-codec" % "1.15"
      ),

    packageName in Docker := "template",
    dockerRepository := {
      if (isSnapshot.value) {
        Some("simplifiersnapshots.azurecr.io")
      } else {
        Some("simplifierdist.azurecr.io")
      }
    },
    dockerAliases += dockerAlias.value.withRegistryHost(None),
    dockerBaseImage := "openjdk:11.0.10-jre-slim-buster",
    dockerExposedVolumes := Seq("/data/template", "/data/transport")
  )
