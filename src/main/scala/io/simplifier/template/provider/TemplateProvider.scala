package io.simplifier.template.provider

import akka.actor.{Actor, ActorLogging, Props, Stash}
import io.simplifier.template.provider.Provider._
import io.simplifier.template.provider.StorageProvider.{NotFound, ReadString, StorageError, StringRead}
import io.simplifier.template.provider.TemplateProvider._

class TemplateProvider(storageProvider: Props) extends Actor with Stash with ActorLogging {

  var templateCache: Map[TemplateForName, CacheEntry[Template]] = Map()
  var error: Map[TemplateForName, TemplateError] = Map()

  override def receive: Receive = {

    case key: TemplateForName if error.contains(key) =>
      log.debug("Received template request in error state.")
      sender() ! error(key)
      error -= key

    case key: TemplateForName if inTemplateCache(key) =>
      log.debug("Received template request with cache hit.")
      sender() ! retrieveFromTemplateCache(key).get

    case key: TemplateForName if loadingTemplate(key) =>
      stash()

    case key@TemplateForName(name, folder) =>
      log.debug("Received template request (need loading).")
      templateCache += key -> Loading
      context.actorOf(storageProvider) ! ReadString(templateName(name, folder))
      stash()

    case StringRead(name, content) =>
      log.debug("Received template content from loader.")
      val key = templateForName(name)
      templateCache += key -> Hit(Template(key.name, key.folder, content))
      unstashAll()

    case storageError@StorageError(name, reason) =>
      log.error(s"Error loading template from storage. Reason: ${reason.getMessage}")
      val key = templateForName(name)
      templateCache -= key
      error += key -> TemplateStorageError(key.name, key.folder, storageError)
      unstashAll()

    case notFoundError@NotFound(name) =>
      log.error(s"Error loading template from storage. Reason: ${notFoundError.getMessage}")
      val key = templateForName(name)
      templateCache -= key
      error += key -> TemplateNotFound(key.name, key.folder, notFoundError)
      unstashAll()

  }

  def loadingTemplate(key: TemplateForName): Boolean = {
    templateCache.get(key).contains(Loading)
  }

  def templateForName(name: String): TemplateForName = {
    val segments = name.split('/')
    TemplateForName(segments(1), segments(0))
  }

  def retrieveFromTemplateCache(key: TemplateForName): Option[Template] = {
    if (inTemplateCache(key)) {
      val Hit(entry, hits) = templateCache(key)
      templateCache += key -> Hit(entry, hits + 1)
      Some(entry)
    } else {
      None
    }
  }

  def inTemplateCache(key: TemplateForName): Boolean = {
    templateCache.get(key).exists(_.isInstanceOf[Hit[Template]])
  }

}

object TemplateProvider {

  val TEMPLATE_FILE_NAME = "template"

  val NAMESPACE = s"${PersistenceAccess.ROOT_NAMESPACE}/templates"

  def templateName(name: String, folder: String) = s"$folder/$name/$TEMPLATE_FILE_NAME"

  def props(): Props = props(StorageProvider.props(NAMESPACE))
  def props(storageProvider: Props): Props = Props(new TemplateProvider(storageProvider))

  trait TemplateError extends Throwable
  case class TemplateStorageError(name: String, folder: String, reason: Throwable)
    extends Throwable(s"Error reading template $folder/$name from storage. Reason: ${reason.getMessage}")
    with TemplateError
  case class TemplateNotFound(name: String, folder: String, reason: Throwable)
    extends Throwable(s"Template $folder/$name does not exists. Details: ${reason.getMessage}")
    with TemplateError



}
