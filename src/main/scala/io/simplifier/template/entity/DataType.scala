package io.simplifier.template.entity

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import io.circe.Json
import io.simplifier.template.entity.DataType._

import scala.util.{Failure, Success, Try}

sealed trait DataType[T] {
  def id: String

  def name: String

  def namespace: Option[String]

  def value(json: Json): Try[T]

  def valueWithContext(json: Json, dataTypes: Map[String, DataType[_]]): Try[T] = value(json)

  def withFailure: PartialFunction[Throwable, Try[T]] = {
    case _ => Failure(InvalidType(name))
  }

  def directlyDependentIds: Set[String] = Set.empty
}

object DataType {

  case object StringType extends DataType[String] {
    val id: String = "22ED1F787B6B0926AB0577860AF7543705341C053EB1B4A74E7CC199A0645E52"
    val name: String = "String"
    val namespace: Option[String] = None

    override def value(json: Json): Try[String] = Try(json.asString.get).recoverWith(withFailure)
  }
  case object DateType extends DataType[String] {
    val id: String = "06A9841478D7BE17C423F11C38CD6829E372093DBEC144F2A85FC7165BE8CD80"
    val name: String = "Date"
    val namespace: Option[String] = None

    override def value(json: Json): Try[String] = {
      val formatter = DateTimeFormatter.ISO_DATE_TIME
      StringType.value(json).map { date =>
        LocalDateTime.parse(date, formatter)
        date
      }.recoverWith(withFailure)
    }
  }
  case object BooleanType extends DataType[Boolean] {
    val id: String = "2788FB5AA776C62635F156C820190D0FD3D558765201881A77382093F7248B39"
    val name: String = "Boolean"
    val namespace: Option[String] = None
    override def value(json: Json): Try[Boolean] = Try(json.asBoolean.get).recoverWith(withFailure)
  }
  case object IntegerType extends DataType[Long] {
    val id: String = "B9B1191E0B70BA0845CF4F6A4F4C017594F8BA84FD2F1849966081D53A8C836D"
    val name: String = "Integer"
    val namespace: Option[String] = None
    override def value(json: Json): Try[Long] = Try(json.asNumber.flatMap(_.toBigDecimal).map(_.toLongExact).get).recoverWith(withFailure)
  }
  case object FloatType extends DataType[Double] {
    val id: String = "C09139C72F5A8A7E0036BA66CE301748BD617F463683EE03F92EDAAAA4AF8BC7"
    val name: String = "Float"
    val namespace: Option[String] = None
    override def value(json: Json): Try[Double] = Try(json.asNumber.map(_.toDouble).get).recoverWith(withFailure)
  }
  case object AnyType extends DataType[Any] {
    val id: String = "D31053204B4A612390A2D6ECDF623E979C14ADC070A7CB9B08B2099C3011BCAB"
    val name: String = "Any"
    val namespace: Option[String] = None
    override def value(json: Json): Try[Any] = Try(converted(json)).recover(withFailure)
  }
  case class DomainType[T](id: String, name: String, namespace: Option[String], derivedFromId: String, constraits: Seq[Constraint]) extends DataType[T] {
    override def value(json: Json): Try[T] = valueWithContext(json, baseDataTypes)

    override def valueWithContext(json: Json, dataTypes: Map[String, DataType[_]]): Try[T] = Try {
      val dataType = dataTypes.getOrElse(derivedFromId, throw UnknownType(derivedFromId)).asInstanceOf[DataType[T]]
      dataType.valueWithContext(json, dataTypes).map { value =>
        constraits.map(_.test(value).get)
        value
      }.get
    }

    override def directlyDependentIds: Set[String] = Set(derivedFromId)
  }

  case class ConstraintViolation(constraint: Constraint)
    extends Throwable(s"Constraint violated: ${constraint.violationText}")
  sealed trait Constraint {
    def fulfilled(testValue: Any): Boolean
    def violationText: String
    def test[T](value: T): Try[T] = {
      if (fulfilled(value)) Success(value) else Failure(ConstraintViolation(this))
    }
  }
  case class Min(value: Int) extends Constraint {
    override def fulfilled(testValue: Any): Boolean = {
      testValue match {
        case l: Long =>
          l >= value
        case str: String =>
          str.length >= value
        case _ => false
      }
    }
    val violationText = s"value below minimum of $value"
  }
  case class Max(value: Int) extends Constraint {
    override def fulfilled(testValue: Any): Boolean = {
      testValue match {
        case l: Long =>
          l <= value
        case str: String =>
          str.length <= value
        case _ => false
      }
    }
    val violationText = s"value exceeds maximum of $value"
  }
  case class Regex(value: String) extends Constraint {
    override def fulfilled(testValue: Any): Boolean = {
      Try(testValue.asInstanceOf[String].matches(value)).getOrElse(false)
    }

    val violationText = s"value does not match regular expression [$value]"
  }
  case class PossibleValues(values: Set[Any]) extends Constraint {
    override def fulfilled(testValue: Any): Boolean = {
      values.contains(testValue)
    }
    val violationText = s"""value not element of { ${values.mkString(", ")} }"""
  }
  case object Nullable extends Constraint {
    override def fulfilled(testValue: Any): Boolean = true
    val violationText = ""
  }
  case class Scale(value: Int) extends Constraint {
    override def fulfilled(testValue: Any): Boolean = {
      Try(BigDecimal(testValue.asInstanceOf[Double]).scale <= value).getOrElse(false)
    }
    val violationText = s"value's scale exceeds $value"
  }
  case class Precision(value: Int) extends Constraint {
    override def fulfilled(testValue: Any): Boolean = {
      Try(BigDecimal(testValue.asInstanceOf[Double]).precision <= value).getOrElse(false)
    }
    val violationText = s"value's precision exceeds $value"
  }
  case class FromDate(value: LocalDateTime) extends Constraint {
    val formatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override def fulfilled(testValue: Any): Boolean = {
      Try {
        val checkDate = LocalDateTime.parse(testValue.asInstanceOf[String], formatter)
        checkDate.isEqual(value) || checkDate.isAfter(value)
      }.getOrElse(false)
    }
    val violationText = s"value before date ${value.format(formatter)}"
  }
  case class DueDate(value: LocalDateTime) extends Constraint {
    val formatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override def fulfilled(testValue: Any): Boolean = {
      Try {
        val checkDate = LocalDateTime.parse(testValue.asInstanceOf[String], formatter)
        checkDate.isEqual(value) || checkDate.isBefore(value)
      }.getOrElse(false)
    }
    val violationText = s"value after date ${value.format(formatter)}"
  }

  case class StructType(id: String, name: String, namespace: Option[String], fields: Map[String, Field]) extends DataType[Map[String, Any]] {
    override def value(json: Json): Try[Map[String,Any]] = valueWithContext(json, baseDataTypes)

    override def valueWithContext(json: Json, dataTypes: Map[String, DataType[_]]): Try[Map[String,Any]] = Try {
      json.asObject.map(_.toMap.filter {
        case (fieldName, value) =>
          val optField = fields.get(fieldName)
          val optional = optField.forall(_.optional)
          optField.isDefined && !(value == Json.Null && optional)
      }.map {
        case (fieldName, fieldValue) =>
          val dataTypeId = fields(fieldName).dataTypeId
          val dataType = dataTypes.getOrElse(dataTypeId, throw UnknownType(dataTypeId))
          val value = dataType.valueWithContext(fieldValue, dataTypes).recoverWith {
            case e: InvalidType =>
              Failure(InvalidTypeForField(fieldName, e))
          }.get
          (fieldName, value)
      }).getOrElse(throw InvalidType(name))
    }

    override def directlyDependentIds: Set[String] = fields.values.map(_.dataTypeId).toSet
  }

  case class Field(name: String, dataTypeId: String, optional: Boolean)

  case class CollectionType(id: String, name: String, namespace: Option[String], elementTypeId: String) extends DataType[Seq[Any]] {
    override def value(json: Json): Try[Seq[Any]] = valueWithContext(json, baseDataTypes)

    override def valueWithContext(json: Json, dataTypes: Map[String, DataType[_]]): Try[Seq[Any]] = Try {
      val dataType = dataTypes.getOrElse(elementTypeId, throw UnknownType(elementTypeId)).asInstanceOf[DataType[Any]]
      json.asArray.getOrElse(throw InvalidType(name)).map { item =>
        dataType.valueWithContext(item, dataTypes).get
      }
    }

    override def directlyDependentIds: Set[String] = Set(elementTypeId)
  }

  case class InvalidTypeForField(field: String, error: InvalidType)
    extends Throwable(s"Type error for field [$field]: ${error.getMessage}")
  case class InvalidType(typeName: String)
    extends Throwable(s"Unexpected value for data type $typeName")
  case class UnknownType(id: String)
    extends Throwable(s"Unknown data type with id $id")

  val baseDataTypes: Map[String, DataType[_]] = Map(
    StringType.id -> StringType,
    BooleanType.id -> BooleanType,
    IntegerType.id -> IntegerType,
    FloatType.id -> FloatType,
    DateType.id -> DateType,
    AnyType.id -> AnyType
  )

  def converted(json: Json): Any = json match {
    case _ if json.isObject =>
      json.asObject.get.toMap.map {
        case (field, value) => field -> converted(value)
      }
    case _ if json.isArray =>
      json.asArray.get.map(converted)
    case _ if json.isNumber =>
      val decimal = json.asNumber.get.toBigDecimal.get
      Try(decimal.toLongExact).getOrElse(decimal.toDouble)
    case _ if json.isString =>
      json.asString.get
    case _ if json.isBoolean =>
      json.asBoolean.get
    case _ if json.isNull => null
    case _ => null
  }

}
