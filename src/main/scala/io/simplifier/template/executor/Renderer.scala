package io.simplifier.template.executor

import java.time.{LocalTime, Duration => JavaDuration}
import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.helper.Helper
import io.simplifier.template.executor.Executor._
import io.simplifier.template.executor.ParameterConverter.{InputForTemplate, ParametersForRendering, ParametersRetreivingError, ParametersValidationError}
import io.simplifier.template.provider.ParameterLoader.ParametersNotFound
import io.simplifier.template.provider.Provider.{Template, TemplateForName}
import io.simplifier.template.provider.TemplateProvider.{TemplateNotFound, TemplateStorageError}

import scala.concurrent.duration.{Duration, NANOSECONDS}


class Renderer(provider: ActorRef, parameterConverter: Props) extends Actor with Stash with ActorLogging {

  import com.gilt.handlebars.scala.binding.dynamic._

  override def receive: Receive = initial

  //val falseHelper = Helper(DynamicBinding())

  var template: Option[Handlebars[Any]] = None
  var convertedParameters: Option[Map[String, Any]] = None
  var timeStartRetrieveRequest: Option[LocalTime] = None //incomming request
  var timeStartRenderTemplate: Option[LocalTime] = None //requirements collected
  var durationRetrieveTemplate: Option[Duration] = None
  var durationParameterConversion: Option[Duration] = None
  var durationTemplateRendering: Option[Duration] = None
  var overallDuration: Option[Duration] = None

  var error: Option[RenderError] = None

  def initial: Receive = {
    case RenderTemplate(name, folder, parameters, requestLabel) =>
      log.debug(s"Received render request")
      stash()
      val now = LocalTime.now
      timeStartRetrieveRequest = Some(now)
      provider ! TemplateForName(name, folder)
      context.actorOf(parameterConverter) ! InputForTemplate(name, folder, parameters)
      context become rendering

    case FetchResponseTime => finalTimings()
  }

  def rendering: Receive = {
    case request: RenderTemplate if error.isDefined =>
      sender() ! error.get.withLabel(label = request.label)
      context stop self
    case Template(_, _, content) =>
      template = Some(Handlebars(content))
      val now = LocalTime.now()
      durationRetrieveTemplate =
        Some(Duration(JavaDuration.between(timeStartRetrieveRequest.get, now).getNano, NANOSECONDS))
      log.debug(s"Time Retrieving Template: ${durationRetrieveTemplate.get}")
      if (done) {
        timeStartRenderTemplate = Some(now)
        unstashAll()
      }
    case ParametersForRendering(name, folder, parameters) =>
      convertedParameters = Some(parameters)
      val now = LocalTime.now()
      durationParameterConversion =
        Some(Duration(JavaDuration.between(timeStartRetrieveRequest.get, now).getNano, NANOSECONDS))
      log.debug(s"Time Converting Parameters: ${durationParameterConversion.get}")
      if (done) {
        timeStartRenderTemplate = Some(now)
        unstashAll()
      }
    case RenderTemplate(_, _, _, label) =>
      val renderedItem = renderTemplate(template.get, convertedParameters.get)
      finalTimings()
      sender() ! RenderedTemplate(renderedItem, label)
      context stop self
    case e@ParametersValidationError(name, folder, _) =>
      finalTimings()
      log.error(s"Error converting parameters. Reason: ${e.getMessage}")
      error = Some(RenderValidationError(name, folder, e, ""))
      unstashAll()
    case e@TemplateStorageError(name, folder, _) =>
      finalTimings()
      log.error(s"Error retrieving template $folder/$name. Reason: ${e.getMessage}")
      error = Some(RenderStorageError(name, folder, e, ""))
      unstashAll()
    case e@TemplateNotFound(name, folder, _) =>
      finalTimings()
      log.debug(s"Error retrieving template $folder/$name. Reason: ${e.getMessage}")
      error = Some(RenderNotFound(name, folder, e, ""))
      unstashAll()
    case e@ParametersRetreivingError(name, folder, reason) =>
      finalTimings()
      reason match {
        case _: ParametersNotFound =>
          log.debug(s"Parameters for template $folder/$name do not exist. Details: ${e.getMessage}")
          error = Some(RenderNotFound(name, folder, e, ""))
        case _ =>
          log.error(s"Error retrieving parameters $folder/$name. Reason: ${e.getMessage}")
          error = Some(RenderStorageError(name, folder, e, ""))
      }
      unstashAll()

  }

  def finalTimings(): Unit = {
    val now = LocalTime.now()
    durationTemplateRendering =
      Some(Duration(JavaDuration.between(timeStartRenderTemplate.getOrElse(now), now).getNano, NANOSECONDS))
    overallDuration =
      Some(Duration(JavaDuration.between(timeStartRetrieveRequest.get, now).getNano, NANOSECONDS))
    context.parent ! FetchResponseTime(durationTemplateRendering.get, overallDuration.get)
    log.debug(s"Time Rendering Template: ${durationTemplateRendering.get}")
    log.debug(s"Time in Renderer: ${overallDuration.get}")
  }

  def done: Boolean = template.isDefined && convertedParameters.isDefined

  def renderTemplate(handlebars: Handlebars[Any], parameters: Map[String, Any]): String = {
    handlebars(parameters)
  }

  override def preStart(): Unit = {
    super.preStart()
    log.debug(s"Start Renderer")
  }

  override def postStop(): Unit = {
    log.debug(s"Stop Renderer")
    super.postStop()
  }

}

object Renderer {

  def props(provider: ActorRef): Props = props(provider, ParameterConverter.props(provider))

  def props(provider: ActorRef, parameterConverter: Props): Props = Props(new Renderer(provider, parameterConverter))


}
