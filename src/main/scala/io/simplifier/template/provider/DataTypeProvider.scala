package io.simplifier.template.provider

import akka.actor.{Actor, ActorLogging, Props, Stash}
import io.simplifier.template.entity.DataType
import io.simplifier.template.provider.DataTypeLoader.{CompleteDataType, DynamicDataTypeLoader, dynamicDataTypeLoader}
import io.simplifier.template.provider.DataTypeProvider._
import io.simplifier.template.provider.Provider._

class DataTypeProvider(dataTypeLoader: DynamicDataTypeLoader)
  extends Actor with Stash with ActorLogging {

  // an import should even invalidate parameter entries if dependent data types are involved
  var dataTypeCache: Map[String, CacheEntry[DataTypeWithDependencies]] = DataType.baseDataTypes.map {
    case (id, dataType) => id -> Hit(DataTypeWithDependencies(dataType, Map(id -> dataType)))
  }

  var errors: Map[String, DataTypeError] = Map()

  override def receive: Receive = {

    case DataTypeById(id, _) if errors.contains(id) =>
      sender() ! errors(id)
      errors -= id

    case DataTypeById(id, _) if inDataTypeCache(id) =>
      log.debug("Received data type request with cache hit.")
      val dataType = retrieveFromDataTypeCache(id).get
      sender() ! dataType

    case dataType: DataTypeById if loadingDataType(dataType.id) =>
      stash()

    case dataType@DataTypeById(id, excludingIds) =>
      log.debug("Received data type request (need loading).")
      val loader = context.actorOf(dataTypeLoader(id, self))
      if (excludingIds.isEmpty) {
        dataTypeCache += id -> Loading
        loader ! dataType
      } else {
        loader forward dataType
      }
      stash()

    case CompleteDataType(dataType, dependencies) =>
      log.debug("Received data type from loader.")
      dataTypeCache += dataType.id -> Hit(DataTypeWithDependencies(dataType, dependencies))
      unstashAll()

    case error: DataTypeError =>
      dataTypeCache -= error.id
      errors += error.id -> error
      unstashAll()

  }

  def inDataTypeCache(key: String): Boolean = {
    dataTypeCache.get(key).exists(_.isInstanceOf[Hit[DataTypeWithDependencies]])
  }

  def retrieveFromDataTypeCache(id: String): Option[DataTypeWithDependencies] = {
    if (inDataTypeCache(id)) {
      val Hit(entry, hits) = dataTypeCache(id)
      dataTypeCache += id -> Hit(entry, hits + 1)
      Some(entry)
    } else {
      None
    }
  }

  def loadingDataType(id: String): Boolean = {
    dataTypeCache.get(id).contains(Loading)
  }


}

object DataTypeProvider {

  def props(): Props = props(dynamicDataTypeLoader)

  def props(dataTypeLoader: DynamicDataTypeLoader): Props = Props(new DataTypeProvider(dataTypeLoader))

  case class DataTypeError(id: String, reason: Throwable)
    extends Throwable(s"Error while reading data type $id. Reason: ${reason.getMessage}")

}
