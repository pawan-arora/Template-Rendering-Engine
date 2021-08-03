package io.simplifier.template.api

import akka.actor.ActorRef
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.Timeout
import io.circe.Json
import io.circe.parser._
import io.simplifier.template.executor.Executor._
import io.simplifier.template.params.{JsonSupport, Parameters}

import scala.concurrent.{ExecutionContext, Future}

class Routes(executor: ActorRef)(implicit timeout: Timeout) extends JsonSupport {

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
      } ~
      post {
        pathPrefix(Segment/Segment) { case(folder, template)  =>

          entity(as[Parameters]) { parameters =>

            val jsonString = parameters.jsonString
            val input = parse(jsonString).getOrElse(Json.Null)
            val renderIt = RenderTemplate(template, folder, input, "")

            extractExecutionContext { implicit ec =>
              onComplete(render(renderIt)) { response =>
                response.map(complete(_)).getOrElse(complete("ERROR"))
              }
            }
          }
        }
      } ~
      get {
        path("fetch"/"time"/"minimum") {
          extractExecutionContext { implicit ec =>
            onComplete(fetchCalculatedValue(CalculateMinimum)) { response =>
                response.map(elm => complete(elm.toString)).getOrElse(complete("ERROR"))
            }
          }
        }
      } ~
      get {
        path("fetch"/"time"/"maximum") {
          extractExecutionContext { implicit ec =>
            onComplete(fetchCalculatedValue(CalculateMaximum)) { response =>
              response.map(elm => complete(elm.toString)).getOrElse(complete("ERROR"))
            }
          }
        }
      } ~
      get {
        path("fetch"/"time"/"average") {
          extractExecutionContext { implicit ec =>
            onComplete(fetchCalculatedValue(CalculateAverage)) { response =>
              response.map(elm => complete(elm.toString)).getOrElse(complete("ERROR"))
            }
          }
        }
      }
   }

  def fetchCalculatedValue(calc: Calculator)(implicit ec: ExecutionContext): Future[Any] = {
    (executor ? calc)
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