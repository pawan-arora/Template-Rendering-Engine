package io.simplifier.template.design.transport

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.{ACursor, HCursor, Json}
import io.simplifier.template.entity.DataType._
import io.simplifier.template.entity.{DataType, Parameter}

import scala.util.{Success, Try}

object Feature {

  case class TransportFeature(id: String, feature: String, name: String, compatibilityVersion: Long, artifactData: Json)
  case class ImportArtifactsData(artifacts: Seq[TransportFeature])

  val DATA = "artifactData"

  val DATATYPE_FEATURE = "com.itizzimo.appServer.transport.feature.DataTypeFeature"
  val TEMPLATE_FEATURE = "com.itizzimo.appServer.transport.feature.TemplateFeature"

  def importArtifactsData(artifacts: String): Try[ImportArtifactsData] = decode[ImportArtifactsData](artifacts).toTry

  def parameters(artifact: String): Try[ParametersFromFeature] = parse(artifact).toTry.flatMap(parameters)

  def parameters(artifact: Json): Try[ParametersFromFeature] = {
    Try(HCursor.fromJson(artifact)
      .downField(DATA).focus.get).flatMap(parametersFromData)
  }

  def parametersFromData(artifactData: Json): Try[ParametersFromFeature] = Try {
    val dataCursor = HCursor.fromJson(artifactData)
    val validate = dataCursor.get[Boolean]("validateIn").toTry.get
    val params = dataCursor
      .downField("params").as[Seq[Parameter]].toTry.get
    ParametersFromFeature(validate, params)
  }

  def dataType(artifact: String): Try[DataType[_]] = parse(artifact).toTry.flatMap(dataType)

  def dataType(artifact: Json): Try[DataType[_]] = {
    val artifactCursor = HCursor.fromJson(artifact)
    val id = artifactCursor.get[String]("id").getOrElse("")
    if (DataType.baseDataTypes contains id) {
      Success(DataType.baseDataTypes(id))
    } else {
      val typeCursor = artifactCursor.downField(DATA)
      dataTypeFromData(typeCursor.focus.get, id)
    }
  }


  def dataTypeFromData(artifactData: Json, id: String): Try[DataType[_]] = Try {
      val typeCursor = HCursor.fromJson(artifactData)
      val name = typeCursor.get[String]("name").getOrElse("")
      val namespace = typeCursor.get[String]("namespace").toOption
      val struct = typeCursor.downField("structType")
      val collection = typeCursor.downField("collectionType")
      val domain = typeCursor.downField("extendedType")
      if (struct.succeeded) {
        val fields = struct.get[Seq[ArtifactStructField]]("fields")
          .map(fields => fields.map(field => Field(field.name, field.fieldType, field.optional))).getOrElse(
          throw FeatureMalFormed(s"Structure parameter [fields] malformed of data type $id")
        )
        StructType(id, name, namespace, fields.map(field => field.name -> field).toMap)
      } else if (collection.succeeded) {
        val elementTypeId = collection.get[String]("collectionType").getOrElse("")
        CollectionType(id, name, namespace, elementTypeId)
      } else {
        val superTypeId = domain.get[String]("parentType").getOrElse("")
        DomainType(id, name, namespace, superTypeId, constraints(domain.downField("properties")))
      }
    }

  def constraints(cursor: ACursor): Seq[Constraint] = {
    cursor.focus.flatMap(_.asObject).map(_.toMap).map { fieldMap => fieldMap.collect {
        case ("Min", value) if isDate(value) =>
          val formatter = DateTimeFormatter.ISO_DATE_TIME
          FromDate(LocalDateTime.parse(value.asString.get, formatter))
        case ("Max", value) if isDate(value) =>
          val formatter = DateTimeFormatter.ISO_DATE_TIME
          DueDate(LocalDateTime.parse(value.asString.get, formatter))
        case ("Min", value) =>
          Min(value.asString.map(_.toInt).getOrElse(0))
        case ("Max", value) =>
          Max(value.asString.map(_.toInt).getOrElse(Integer.MAX_VALUE))
        case ("Nullable", value) if value.asString.contains("true")=>
          Nullable
        case ("Scale", value) =>
          Scale(value.asString.map(_.toInt).getOrElse(0))
        case ("Precision", value) =>
          Precision(value.asString.map(_.toInt).getOrElse(0))
        case ("Regex", value) =>
          Regex(value.asString.getOrElse(""))
        case ("Values", value) =>
          PossibleValues(value.asString.map(parse).flatMap(_.toOption).flatMap(_.asArray).map(_.map(AnyType.value).map(_.getOrElse(null))).getOrElse(Seq()).toSet)
      }
    }.map(_.toSeq).getOrElse(Seq())
  }

  def isDate(json: Json): Boolean = {
    val formatter = DateTimeFormatter.ISO_DATE_TIME
    Try(LocalDateTime.parse(json.asString.get, formatter)).isSuccess
  }

  def isDouble(json: Json):Boolean = {
    json.asNumber.isDefined
  }

  def templateName(artifact: TransportFeature): Try[TemplateName] = Try {
    val cursor = HCursor.fromJson(artifact.artifactData)
    val folder = cursor.downField("folder").focus.flatMap(_.asString).get
    val name = artifact.name
    TemplateName(name, folder)
  }

  case class TemplateName(name: String, folder: String) {
    override def toString: String = s"$folder/$name"
  }
  case class ArtifactStructField(name: String, fieldType: String, optional: Boolean)
  case class ParametersFromFeature(validate: Boolean, parameters: Seq[Parameter])
  case class FeatureMalFormed(reason: String) extends Throwable(s"Feature malformed: $reason.")

}
