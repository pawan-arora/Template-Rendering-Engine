package io.simplifier.template.provider

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import io.simplifier.template.design.transport.Feature
import io.simplifier.template.design.transport.Feature.ParametersFromFeature
import io.simplifier.template.entity.Parameter
import io.simplifier.template.provider.DataTypeProvider.DataTypeError
import io.simplifier.template.provider.ParameterLoader._
import io.simplifier.template.provider.Provider._
import io.simplifier.template.provider.StorageProvider.{NotFound, ReadString, StorageError, StringRead}

class ParameterLoader(name: String, folder: String, storageProvider: Props, dataTypeProvider: ActorRef)
  extends Actor with Stash with ActorLogging {


  var parameters: Option[Map[String, Parameter]] = None
  var validate: Boolean = true
  var dataTypes: Map[String, CacheEntry[DataTypeWithDependencies]] = Map()
  var error: Option[ParametersError] = None

  override def preStart(): Unit = {
    super.preStart()
    log.debug(
      s"""ParameterLoader started: ${self.path.toSerializationFormat}
         |with DataTypeProvider ${dataTypeProvider.path.toSerializationFormat}""".stripMargin)
  }

  override def receive: Receive = {

    case ParametersForTemplate(_, _) if error.isDefined =>
      sender() ! error.get
      context stop self

    case ParametersForTemplate(name, folder) if done =>
      log.debug("Received parameters request after collecting.")
      val dataTypeMap = dataTypes.values.flatMap {
        case Hit(dataTypeResult, _) =>
          dataTypeResult.dependencies
        case _ =>
          throw PreMatureReturn
      }.toMap
      sender() ! ParametersWithDataTypes(name, folder, validate, parameters.get.values.toSeq, dataTypeMap)
      context stop self

    case ParametersForTemplate(name, folder) =>
      log.debug("Received parameters request (need loading).")
      context.actorOf(storageProvider) ! ReadString(parametersName(name, folder))
      stash()

    case StringRead(_, content) =>
      log.debug("Received parameters from storage access.")
      Feature.parameters(content).foreach {
        case ParametersFromFeature(definedValidate, parameterSequence) =>
          validate = definedValidate
          parameters = Some(parameterSequence.map(p => p.name -> p).toMap)
          parameterSequence.map(_.dataTypeId).toSet[String].foreach { dataTypeId =>
            dataTypes += dataTypeId -> Loading
            log.debug("Request data type $dataTypeId.")
            dataTypeProvider ! DataTypeById(dataTypeId)
          }
          if (parameterSequence.isEmpty) {
            unstashAll()
          }
      }

    case dt@DataTypeWithDependencies(dataType, _) =>
      log.debug("Received a data type from provider.")
      dataTypes += dataType.id -> Hit(dt)
      if (done) {
        unstashAll()
      }

    case e: StorageError =>
      log.error(s"Error loading parameters from storage. Reason: ${e.getMessage}")
      error = Some(ParametersStorageError(name, folder, e))
      unstashAll()

    case e: NotFound =>
      log.debug(s"Parameters not found: ${e.getMessage}")
      error = Some(ParametersNotFound(name, folder))
      unstashAll()

    case e: DataTypeError =>
      log.error(s"Error loading data type from storage. Reason: ${e.getMessage}")
      error = Some(ParametersStorageError(name, folder, e))
      unstashAll()
  }

  def done: Boolean = parameters.isDefined && !dataTypes.exists(_._2 == Loading)

}

object ParameterLoader {

  val PARAMETERS_FILE_NAME = "artifact.json"

  val NAMESPACE: String = TemplateProvider.NAMESPACE

  def parametersName(name: String, folder: String): String = s"$folder/$name/$PARAMETERS_FILE_NAME"

  type DynamicParameterLoader = (String, String, ActorRef) => Props

  def dynamicParameterLoader(name: String, folder: String, dataTypeProvider: ActorRef): Props = props(name, folder, dataTypeProvider)

  def props(name: String, folder: String, dataTypeProvider: ActorRef): Props =
    props(name, folder, dataTypeProvider, StorageProvider.props(NAMESPACE))
  def props(name: String, folder: String, dataTypeProvider: ActorRef, storageProvider: Props): Props =
    Props(new ParameterLoader(name, folder, storageProvider, dataTypeProvider))

  case object PreMatureReturn extends Throwable("Returning of parameters with unfinished datatype collection")

  trait ParametersError extends Throwable
  case class ParametersStorageError(name: String, folder: String, reason: Throwable)
    extends Throwable(s"Unable to retrieve parameters for template $folder/$name. Reason: ${reason.getMessage}")
    with ParametersError
  case class ParametersNotFound(name: String, folder: String)
    extends Throwable(s"Parameters for template $folder/$name do not exist.")
    with ParametersError

}
