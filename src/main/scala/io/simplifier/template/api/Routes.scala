package io.simplifier.template.api

import akka.actor.ActorRef
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.util.Timeout
import io.circe.Json
import io.simplifier.template.executor.Executor.{RenderTemplate, RenderedTemplate}
import io.circe.parser._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class Routes(executor: ActorRef)(implicit timeout: Timeout) {

  lazy val route: Route = {
    pathEndOrSingleSlash {
      complete("Template Node up and running")
    } ~
      path(Segment/Segment) { case (folder, template) =>
        get {
          val appName = "MyApp"
          val email = "my@email.com"
          val invitationUrl = "http://invitation/url"
          val jsonString =
            s"""{
              |   "AppName": "$appName",
              |   "email": "$email",
              |   "invitationUrl": "$invitationUrl"
              |}""".stripMargin
          val input = parse(jsonString).getOrElse(Json.Null)
          val renderIt = RenderTemplate(template, folder, input, "")

          extractExecutionContext { implicit ec =>
            onComplete(render(renderIt)) { response =>
              response.map(complete(_)).getOrElse(complete("ERROR"))
            }
          }

        }
      }
  }

  def render(renderIt: RenderTemplate)(implicit ec: ExecutionContext): Future[String] = {
    (executor ? renderIt).map {
      case RenderedTemplate(item, _) =>
        item
      case t: Throwable =>
        throw t
    }
  }


}