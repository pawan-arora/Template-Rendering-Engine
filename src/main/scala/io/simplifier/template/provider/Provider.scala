package io.simplifier.template.provider

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Stash}
import io.simplifier.template.entity.{DataType, Parameter}
import io.simplifier.template.provider.Provider.{DataTypeById, ParametersForTemplate, TemplateForName}

class Provider private (templateProvider: ActorRef, parametersProvider: ActorRef, dataTypeProvider: ActorRef)
  extends Actor with Stash with ActorLogging {

  override def preStart(): Unit = {
    super.preStart()
    log.debug(s"Provider started: ${self.path.toSerializationFormat}")
  }

  override def receive: Receive = {

    case template: TemplateForName =>
      log.debug("Received template request.")
      templateProvider forward template

    case parameters: ParametersForTemplate =>
      log.debug("Received parameters request.")
      parametersProvider forward parameters

    case dataType: DataTypeById =>
      log.debug("Received data type request.")
      dataTypeProvider forward dataType

  }





}

object Provider {

  private var templateProvider: Option[ActorRef] = None
  private var parameterProvider: Option[ActorRef] = None
  private var dataTypeProvider: Option[ActorRef] = None
  private var provider: Option[ActorRef] = None

  def start(system: ActorSystem): ActorRef = {
    val t = templateProvider.getOrElse({
      templateProvider = Some(system.actorOf(TemplateProvider.props()))
      templateProvider.get
    })
    val d = dataTypeProvider.getOrElse({
      dataTypeProvider = Some(system.actorOf(DataTypeProvider.props()))
      dataTypeProvider.get
    })
    val p = parameterProvider.getOrElse({
      parameterProvider = Some(system.actorOf(ParameterProvider.props(d)))
      parameterProvider.get
    })
    provider.getOrElse({
      provider = Some(system.actorOf(props(t, p, d)))
      provider.get
    })
  }


  def props(templateProvider: ActorRef,
            parameterProvider: ActorRef,
            dataTypeProvider: ActorRef): Props =
    Props(new Provider(templateProvider, parameterProvider, dataTypeProvider))

  sealed trait CacheEntry[+T]
  case object Loading extends CacheEntry[Nothing]
  case class Hit[+T](entry: T, hits: Long = 0) extends CacheEntry[T]

  // Requests
  case class TemplateForName(name: String, folder: String)
  case class ParametersForTemplate(name: String, folder: String)
  case class DataTypeById(id: String, excludingIds: Set[String] = Set())
  case class DataTypesById(ids: Set[String])

  // Responses
  case class Template(name: String, folder: String, template: String)
  case class ParametersWithDataTypes(name: String, folder: String, validate: Boolean, parameters: Seq[Parameter], dataTypes: Map[String, DataType[_]])
  case class DataTypeWithDependencies(dataType: DataType[_], dependencies: Map[String, DataType[_]])

}
