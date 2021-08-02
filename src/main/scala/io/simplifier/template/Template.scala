package io.simplifier.template

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import io.simplifier.template.api.Routes
import io.simplifier.template.executor.Executor
import io.simplifier.template.node.TemplateNode
import io.simplifier.template.provider._

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.{Failure, Success, Try}

object Template extends App with LazyLogging {

  val config = ConfigFactory.load()
  val akkaManagementPort = Try(config.getInt("akka.management.http.port")).getOrElse(8558)
  val akkaManagementHost = Try(config.getString("akka.management.http.hostname")).getOrElse("localhost")
  val akkaHostname = Try(config.getString("akka.remote.artery.canonical.hostname")).getOrElse("localhost")
  val akkaPort = Try(config.getInt("akka.remote.artery.canonical.port")).getOrElse(2551)
  logger.info(
    s"""
       |************************************************************
       |*        This is a Template Node
       |* Management: $akkaManagementHost:$akkaManagementPort
       |* Remote:     $akkaHostname:$akkaPort
       |************************************************************
       |""".stripMargin)

  FileSystemAccess.initNamespaces(DataTypeLoader.NAMESPACE)
    .flatMap(_ => FileSystemAccess.initNamespaces(TemplateProvider.NAMESPACE)) match {
    case Success(_) =>
      logger.info("Initialized File System")
    case Failure(exception) =>
      logger.error(s"Initialization of File System failed: ${exception.getMessage}")
      exception.printStackTrace()
  }

  ProvidedTemplatesInstaller.installProvidedTemplates

  implicit val system: ActorSystem = ActorSystem("simplifier", config)

  val provider = Provider.start(system)
  logger.debug("Provider started.")
  val executor = Executor.start(system, provider)
  logger.debug("Executor started.")
  TemplateNode.start(system)
  logger.debug("Node started.")

  Http()
    .newServerAt("0.0.0.0", 8008)
    .bind(new Routes(executor)(Timeout(1.minute)).route)

  Await.result(system.whenTerminated, Duration.Inf)

}
