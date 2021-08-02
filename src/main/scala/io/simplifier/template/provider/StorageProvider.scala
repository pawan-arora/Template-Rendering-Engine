package io.simplifier.template.provider

import akka.actor.{Actor, ActorLogging, Props}
import io.simplifier.template.provider.PersistenceAccess.ItemNotFound
import io.simplifier.template.provider.StorageProvider._

import scala.util.Failure

class StorageProvider(access: PersistenceAccess)
  extends Actor with ActorLogging {

  override def receive: Receive = {
    case ReadString(name) =>
      log.debug(s"Received read request $name.")
      access.readString(name)
        .recoverWith { error =>
          log.error(s"Error while reading data. Reason: ${error.getMessage}")
          sender() ! errorMessage(error, name)
          Failure(error)
        }
        .foreach { content =>
          sender() ! StringRead(name, content)
        }
    case ReadBytes(name) =>
      access.readBytes(name)
        .recoverWith { error =>
          log.error(s"Error while reading data. Reason: ${error.getMessage}")
          sender() ! errorMessage(error, name)
          Failure(error)
        }
        .foreach { content =>
          sender() ! BytesRead(name, content)
        }

  }

  def errorMessage(error: Throwable, name: String): StorageProviderMessage = {
    error match {
      case _: ItemNotFound =>
        NotFound(name)
      case _ =>
        StorageError(name, error)
    }
  }

}

object StorageProvider {

  def props(namespace: String): Props = props(FileSystemAccess(namespace))

  def props(access: PersistenceAccess): Props = Props(new StorageProvider(access)).withDispatcher("io-dispatcher")

  sealed trait StorageProviderMessage
  case class ReadString(name: String) extends StorageProviderMessage
  case class ReadBytes(name: String) extends StorageProviderMessage
  case class StringRead(name: String, content: String) extends StorageProviderMessage
  case class BytesRead(name: String, bytes: Array[Byte]) extends StorageProviderMessage
  case class StorageError(name: String, reason: Throwable)
    extends Throwable(s"Error loading entity $name from storage.") with StorageProviderMessage
  case class NotFound(name: String)
    extends Throwable(s"Item $name not found in storage.") with StorageProviderMessage

}
