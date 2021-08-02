package io.simplifier.template.provider

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import io.simplifier.template.design.transport.Feature
import io.simplifier.template.entity.DataType
import io.simplifier.template.entity.DataType.{CollectionType, DomainType, StructType}
import io.simplifier.template.provider.DataTypeLoader._
import io.simplifier.template.provider.DataTypeProvider.DataTypeError
import io.simplifier.template.provider.Provider._
import io.simplifier.template.provider.StorageProvider.{NotFound, ReadString, StorageError, StringRead}

import scala.util.Failure

class DataTypeLoader(dataTypeId: String, storageProvider: Props, dataTypeProvider: ActorRef)
  extends Actor with Stash with ActorLogging {

  var excludingIdsCache: Set[String] = Set()
  var dependenciesToCollect: Map[String, CacheEntry[DataType[_]]] = Map()
  var directDependecies: Set[String] = Set()
  var allDependencies: Map[String, DataType[_]] = Map()
  var loadResult: Option[DataTypeResult] = None
  var cachedDataType: Option[DataType[_]] = None
  var error: Option[DataTypeError] = None

  override def receive: Receive = {

    case _: DataTypeById if error.isDefined =>
      sender() ! error.get
      context stop self

    case dataType: DataTypeById if loadResult.isDefined =>
      log.debug("Received data type request after collecting all.")
      sender() ! loadResult.get
      context stop self

    case DataTypeById(id, excludingIds) =>
      log.debug("Received data type request (need loading).")
      excludingIdsCache ++= (excludingIds + id)
      context.actorOf(storageProvider) ! ReadString(dataTypeName(id))
      stash()

    case StringRead(_, content) =>
      log.debug("Received data type from access.")
      Feature.dataType(content).recoverWith {
        case parsingError =>
          log.error(s"Parsing of data type $dataTypeId failed.")
          error = Some(DataTypeError(dataTypeId, parsingError))
          unstashAll()
          Failure(parsingError)
      }.foreach { dataType =>
        cachedDataType = Some(dataType)
        directDependecies = dependentIds(dataType)
        allDependencies += dataType.id -> dataType
        dependenciesToCollect = (directDependecies diff excludingIdsCache).map(id => id -> Loading).toMap
        if (directDependecies.isEmpty || directDependecies == Set(dataType.id)) {
          loadResult = Some(CompleteDataType(dataType, allDependencies))
          unstashAll()
        } else if (dependenciesToCollect.isEmpty) {
          loadResult = Some(IncompleteDataType(dataType, allDependencies, excludingIdsCache))
          unstashAll()
        } else {
          dependenciesToCollect.keys.foreach { dependentDataType =>
            log.debug(s"Request dependent data type $dependentDataType.")
            dataTypeProvider ! DataTypeById(dependentDataType, excludingIdsCache)
          }
        }
      }

    case result: DataTypeResult =>
      log.debug(s"Loader of ${cachedDataType.get.name} received data type from other loader. ${result.dataType.name}")
      dependenciesToCollect += result.dataType.id -> Hit(result.dataType)
      allDependencies ++= result.dependencies
      if (doneLoadingDependencies) {
        loadResult = loadResultValue
        unstashAll()
      }

    case DataTypeWithDependencies(dataType, dependencies) =>
      log.debug(s"Loader of ${cachedDataType.get.name} received data type from cache. ${dataType.name}")
      dependenciesToCollect += dataType.id -> Hit(dataType)
      allDependencies ++= dependencies
      if (doneLoadingDependencies) {
        loadResult = loadResultValue
        unstashAll()
      }

    case e: DataTypeError =>
      log.error(s"Error loading data type from storage. Reason: ${e.getMessage}")
      // errors on dependent data types lead to own failure
      error = Some(DataTypeError(dataTypeId, e))
      unstashAll()

    case e: StorageError =>
      log.error(s"Error loading data type from storage. Reason: ${e.getMessage}")
      error = Some(DataTypeError(dataTypeId, e))
      unstashAll()

    case e: NotFound =>
      log.error(s"Error loading data type from storage. Reason: ${e.getMessage}")
      error = Some(DataTypeError(dataTypeId, e))
      unstashAll()
  }

  def loadResultValue: Option[DataTypeResult] = {
    if (isComplete) {
      Some(CompleteDataType(cachedDataType.get, allDependencies))
    } else {
      Some(IncompleteDataType(cachedDataType.get, allDependencies, excludingIdsCache))
    }
  }

  def doneLoadingDependencies: Boolean = dependenciesToCollect.values.forall(_.isInstanceOf[Hit[_]])

  def isComplete: Boolean = (directDependecies subsetOf allDependencies.keySet) && cachedDataType.isDefined && excludingIdsCache == Set(cachedDataType.get.id)

  def dependentIds(dataType: DataType[_]): Set[String] = dataType match {
    case DomainType(_, _, _, derivedFromId, _) =>
      Set(derivedFromId)
    case CollectionType(_, _, _, elementTypeId) =>
      Set(elementTypeId)
    case StructType(_, _, _, fields) =>
      fields.values.map(_.dataTypeId).toSet
    case _ => Set()
  }

}

object DataTypeLoader {

  val NAMESPACE = s"${PersistenceAccess.ROOT_NAMESPACE}/datatypes"

  type DynamicDataTypeLoader = (String, ActorRef) => Props

  def dynamicDataTypeLoader(dataTypeId: String, provider: ActorRef): Props = props(dataTypeId, provider)

  def props(dataTypeId: String, dataTypeProvider: ActorRef): Props =
    props(dataTypeId, dataTypeProvider, StorageProvider.props(NAMESPACE))

  def props(dataTypeId: String, dataTypeProvider: ActorRef, storageProvider: Props): Props =
    Props(new DataTypeLoader(dataTypeId, storageProvider, dataTypeProvider))

  def dataTypeName(dataTypeId: String): String = dataTypeId

  sealed trait DataTypeResult {
    def dataType: DataType[_]
    def dependencies: Map[String, DataType[_]]
  }
  case class CompleteDataType(dataType: DataType[_], dependencies: Map[String, DataType[_]]) extends DataTypeResult
  case class IncompleteDataType(dataType: DataType[_], dependencies: Map[String, DataType[_]], excludingIds: Set[String]) extends DataTypeResult

}
