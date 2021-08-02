package io.simplifier.template.provider

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import io.simplifier.template.provider.ParameterLoader.{DynamicParameterLoader, ParametersError, ParametersNotFound, ParametersStorageError}
import io.simplifier.template.provider.Provider._

class ParameterProvider(dataTypeProvider: ActorRef, parameterLoader: DynamicParameterLoader)
  extends Actor with Stash with ActorLogging {

  // an import should even check changes in data types to invalidate entries
  var parametersCache: Map[ParametersForTemplate, CacheEntry[ParametersWithDataTypes]] = Map()
  var errors: Map[ParametersForTemplate, ParametersError] = Map()

  override def receive: Receive = {

    case request: ParametersForTemplate if errors.contains(request) =>
      log.debug("Send error to requester")
      sender() ! errors(request)
      errors -= request

    case request: ParametersForTemplate if inParametersCache(request) =>
      log.debug("Received parameters request with cache hit.")
      val parameters = retrieveFromParametersCache(request).get
      sender() ! parameters

    case request: ParametersForTemplate if loadingParameters(request) =>
      stash()

    case request@ParametersForTemplate(name, folder) =>
      log.debug("Received parameters request (need loading).")
      context.actorOf(parameterLoader(name, folder, dataTypeProvider)) ! request
      stash()

    case loadedParams@ParametersWithDataTypes(name, folder, _, _, _) =>
      log.debug("Received parameters from loader.")
      parametersCache += ParametersForTemplate(name, folder) -> Hit(loadedParams)
      unstashAll()

    case error@ParametersStorageError(name, folder, _) =>
      log.error("Received storage error from loader.")
      val key = ParametersForTemplate(name, folder)
      parametersCache -= key
      errors += key -> error
      unstashAll()

    case error@ParametersNotFound(name, folder) =>
      log.debug(s"Parameters $name not found.")
      val key = ParametersForTemplate(name, folder)
      parametersCache -= key
      errors += key -> error
      unstashAll()

  }

  def inParametersCache(key: ParametersForTemplate): Boolean = {
    parametersCache.get(key).exists(_.isInstanceOf[Hit[ParametersWithDataTypes]])
  }

  def retrieveFromParametersCache(key: ParametersForTemplate): Option[ParametersWithDataTypes] = {
    if (inParametersCache(key)) {
      val Hit(entry, hits) = parametersCache(key)
      parametersCache += key -> Hit(entry, hits + 1)
      Some(entry)
    } else {
      None
    }
  }

  def loadingParameters(key: ParametersForTemplate): Boolean = {
    parametersCache.get(key).contains(Loading)
  }


}

object ParameterProvider {

  def props(dataTypeProvider: ActorRef): Props = props(dataTypeProvider, ParameterLoader.dynamicParameterLoader)
  def props(dataTypeProvider: ActorRef, parameterLoader: DynamicParameterLoader): Props =
    Props(new ParameterProvider(dataTypeProvider, parameterLoader))

}
