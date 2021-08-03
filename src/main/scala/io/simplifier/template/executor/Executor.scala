package io.simplifier.template.executor

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import io.circe.Json
import io.simplifier.template.executor.Executor._

import scala.concurrent.duration.Duration

class Executor(provider: ActorRef, renderer: Props) extends Actor {

  private var minimum: Long = Int.MaxValue
  private var maximum: Long = 0
  private var average: Long = 0
  private var count: Long = 1

  override def receive: Receive = {
    case request: RenderTemplate =>
      val renderActor = context.actorOf(renderer)
      renderActor forward request

    case request: FetchResponseTime =>
      minimum = minimum.min(request.overallDuration._1)
      maximum = maximum.max(request.overallDuration._1)
      count = count + 1
      average = (minimum + maximum) / count

    case CalculateMinimum => sender() ! minimum

    case CalculateMaximum => sender() ! maximum

    case CalculateAverage => sender() ! average
  }

}

object Executor {

  private var executor: Option[ActorRef] = None

  def start(system: ActorSystem, provider: ActorRef): ActorRef = {
    executor.getOrElse({
      executor = Some(system.actorOf(props(provider)))
      executor.get
    })
  }

  def props(provider: ActorRef): Props = props(provider, Renderer.props(provider))

  def props(provider: ActorRef, renderer: Props): Props = Props(new Executor(provider, renderer))

  case class RenderTemplate(name: String, folder: String, input: Json, label: String)
  case class FetchResponseTime(durationTemplateRendering: Duration, overallDuration: Duration)
  case class RenderedTemplate(item: String, label: String)

  sealed trait Calculator
  case object CalculateMinimum extends Calculator
  case object CalculateMaximum extends Calculator
  case object CalculateAverage extends Calculator

  sealed trait RenderError extends Throwable { self =>

    def withLabel(label: String): RenderError = this match {
      case storage: RenderStorageError => storage.copy(label = label)
      case notFound: RenderNotFound => notFound.copy(label = label)
      case validation: RenderValidationError => validation.copy(label = label)
    }
  }
  case class RenderStorageError(name: String, folder: String, e: Throwable, label: String)
    extends Throwable(s"Error on rendering $folder/$name. Reason: ${e.getMessage}")
    with RenderError
  case class RenderNotFound(name: String, folder: String, e: Throwable, label: String)
    extends Throwable(s"Template $folder/$name not found. Details: ${e.getMessage}")
    with RenderError
  case class RenderValidationError(name: String, folder: String, e: Throwable, label: String)
    extends Throwable(s"Validation of input for Template $folder/$name failed. Details: ${e.getMessage}")
    with RenderError

}
