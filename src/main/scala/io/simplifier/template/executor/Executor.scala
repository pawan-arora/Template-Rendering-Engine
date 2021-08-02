package io.simplifier.template.executor

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import io.circe.Json
import io.simplifier.template.executor.Executor._

class Executor(provider: ActorRef, renderer: Props) extends Actor {

  override def receive: Receive = {
    case request: RenderTemplate =>
      val renderActor = context.actorOf(renderer)
      renderActor forward request

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
  case class RenderedTemplate(item: String, label: String)
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
