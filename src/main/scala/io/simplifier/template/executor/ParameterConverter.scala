package io.simplifier.template.executor

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import io.circe.Json
import io.simplifier.template.entity.DataType.AnyType
import io.simplifier.template.entity.Parameter
import io.simplifier.template.executor.ParameterConverter._
import io.simplifier.template.provider.ParameterLoader.{ParametersError, ParametersNotFound, ParametersStorageError}
import io.simplifier.template.provider.Provider.{ParametersForTemplate, ParametersWithDataTypes}

import scala.util.{Failure, Success, Try}

class ParameterConverter(provider: ActorRef)
  extends Actor with Stash with ActorLogging {

  var collectedParameters: Option[ParametersWithDataTypes] = None
  var error: Option[ParametersError] = None

  override def receive: Receive = {

    case InputForTemplate(name, folder, _) if error.isDefined =>
      log.debug("Send error to requester")
      sender() ! ParametersRetreivingError(name, folder, error.get)
      context stop self

    case InputForTemplate(name, folder, parameters) if parametersCollected =>
      log.debug("Received Input Request with cache hit.")
      val answer = if (collectedParameters.get.validate) {
        log.debug("Perform typed conversion.")
        typedConversion(parameters) match {
          case Success(params) => ParametersForRendering(name, folder, params)
          case Failure(e) =>
            log.debug(s"Error validating input against parameters. Reason: ${e.getMessage}")
            ParametersValidationError(name, folder, e)
        }
      } else {
        log.debug("Perform typeless conversion.")
        ParametersForRendering(name, folder, typelessConversion(parameters).get)
      }
      sender() ! answer
      context stop self


    case InputForTemplate(name, folder, _) =>
      log.debug("Received Input Request (need loading).")
      provider ! ParametersForTemplate(name, folder)
      stash()

    case parameters: ParametersWithDataTypes =>
      log.debug("Received paratemeters.")
      collectedParameters = Some(parameters)
      unstashAll()

    case e@ParametersStorageError(name, folder, reason) =>
      log.error(s"Error loading parameters from storage. Reason: ${reason.getMessage}")
      error = Some(e)
      unstashAll()

    case e@ParametersNotFound(name, folder) =>
      log.debug(s"Parameters $name not found. Details: ${e.getMessage}")
      error = Some(e)
      unstashAll()

  }

  def parametersCollected: Boolean = collectedParameters.isDefined

  def typedConversion(parameters: Json): Try[Map[String, Any]] = Try {
    val parameterMap = (for {
      dataTypes <- collectedParameters.map(_.dataTypes)
      params <- collectedParameters.map(_.parameters)
      fieldMap <- parameters.asObject.map(_.toMap)
    } yield Try {
      val optionalParams = params.filter(_.optional).map(_.name).toSet
      normalized(fieldMap)(params)
        .filterNot(nullAndOptional(optionalParams))
        .filter { case (fieldName, _) => params.exists(_.name == fieldName) }
        .map {
          case (fieldName, value) =>
            val dataTypeId = params.find(_.name == fieldName).map(_.dataTypeId).getOrElse(throw UnexpectedParameter(fieldName))
            val dataType = dataTypes(dataTypeId)
            val convertedValue = dataType.valueWithContext(value, dataTypes)
              .recoverWith {
                case error =>
                  Failure(InvalidParameter(fieldName, error))
              }.get
            (fieldName, convertedValue)
      } ++ constValues(params)
    }.get).getOrElse(Map())
    val notOptionalParameterNames = collectedParameters.get.parameters.filter(!_.optional).map(_.name).toSet
    if (notOptionalParameterNames subsetOf parameterMap.keySet) {
      parameterMap
    } else {
      val missingParameterName = (notOptionalParameterNames diff parameterMap.keySet).head
      val missingParameter = collectedParameters.get.parameters.find(_.name == missingParameterName).get
      throw MissingParameter(missingParameter)
    }
  }

  def nullAndOptional(optionals: Set[String])(kv: (String, Json)): Boolean = kv match {
    case (fieldName, value) =>
      value.isNull && optionals.contains(fieldName)
  }

  def constValues(parameter: Seq[Parameter]): Map[String, Any] = {
    parameter.collect {
      case Parameter(name, _, Some(value), _, _, _) =>
        (name, value)
    }.toMap
  }

  def typelessConversion(parameters: Json): Try[Map[String, Any]] = Try {
    (for {
      params <- collectedParameters.map(_.parameters)
      fieldMap <- parameters.asObject.map(_.toMap)
    } yield {
      normalized(fieldMap)(params).map{
        case (fieldName, fieldValue) =>
          (fieldName, AnyType.value(fieldValue).get)
      }
    }).getOrElse(Map()) ++ constValues(collectedParameters.map(_.parameters).getOrElse(Seq()))
  }

  def normalized(fieldMap: Map[String, Json])(implicit parameters: Seq[Parameter]): Map[String, Json] = {
    fieldMap.map {
      case (fieldName, value) =>
        val normalizedName = parameters.collectFirst {
          case parameter if parameter.alias contains fieldName =>
            parameter.name
        }.getOrElse(fieldName)
        (normalizedName, value)
    }
  }

}

object ParameterConverter {

  def props(provider: ActorRef): Props =  Props(new ParameterConverter(provider))

  case class InputForTemplate(name: String, folder: String, input: Json)
  case class ParametersForRendering(name: String, folder: String, parameters: Map[String, Any])
  case class MissingParameter(parameter: Parameter)
    extends Throwable({
    val alias = parameter.alias.map(a => s""" (aka "$a")""").getOrElse("")
    s"""Did not find expected parameter "${parameter.name}"$alias."""
  })
  case class UnexpectedParameter(parameterName: String)
    extends Throwable(s"Unexpected parameter [$parameterName]")
  case class InvalidParameter(parameterName: String, reason: Throwable)
    extends Throwable(s"Validation for parameter [$parameterName] failed: Reason: ${reason.getMessage}")
  case class ParametersValidationError(name: String, folder: String, reason: Throwable)
    extends Throwable(s"Validation failed for template $folder/$name. Reason: ${reason.getMessage}")
  case class ParametersRetreivingError(name: String, folder: String, reason: Throwable)
    extends Throwable(s"Unable to get parametes of template $folder/$name. Reason: ${reason.getMessage}")

}
