package io.simplifier.template.entity

import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import io.circe.Json
import org.apache.commons.io.IOUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.circe.parser._

import scala.util._

class DataTypeTest extends AnyFlatSpec with Matchers {

  val addressArtifact = IOUtils.resourceToString("/address.json", StandardCharsets.UTF_8)
  val addressesArtifact = IOUtils.resourceToString("/addresses.json", StandardCharsets.UTF_8)
  val person = Feature.dataType(IOUtils.resourceToString("/person.json", StandardCharsets.UTF_8)).get
  val char = Feature.dataType(IOUtils.resourceToString("/char.json", StandardCharsets.UTF_8)).get
  val christmas21 = Feature.dataType(IOUtils.resourceToString("/christmas21.json", StandardCharsets.UTF_8)).get
  val digit = Feature.dataType(IOUtils.resourceToString("/digit.json", StandardCharsets.UTF_8)).get
  val modulo11 = Feature.dataType(IOUtils.resourceToString("/modulo11.json", StandardCharsets.UTF_8)).get
  val rgb = Feature.dataType(IOUtils.resourceToString("/rgb.json", StandardCharsets.UTF_8)).get

  val formatter = DateTimeFormatter.ISO_DATE_TIME


  "A DataType" should "convert a JSON value into a String value" in {
    StringType.value(Json.fromString("a string")) shouldBe Success("a string")
  }

  it should "fail converting a JSON boolean into a String value" in {
    StringType.value(Json.fromBoolean(true)) shouldBe Failure(InvalidType("String"))
  }

  it should "convert a JSON value into a Boolean value" in {
    BooleanType.value(Json.fromBoolean(true)) shouldBe Success(true)
  }

  it should "fail converting a JSON string into a Boolean value" in {
    BooleanType.value(Json.fromString("true")) shouldBe Failure(InvalidType("Boolean"))
  }

  it should "convert a JSON value into an Integer value" in {
    IntegerType.value(Json.fromLong(123)) shouldBe Success(123)
  }

  it should "fail converting a JSON string into a Integer value" in {
    IntegerType.value(Json.fromString("true")) shouldBe Failure(InvalidType("Integer"))
  }

  it should "convert a JSON value into an Float value" in {
    FloatType.value(Json.fromDouble(123.125).get) shouldBe Success(123.125)
  }

  it should "fail converting a JSON string into a Float value" in {
    FloatType.value(Json.fromString("true")) shouldBe Failure(InvalidType("Float"))
  }


  it should "convert a JSON value in a native value" in {

    val result = Feature.dataType(addressArtifact)
    result.isSuccess shouldBe true

    val dataType = result.get.asInstanceOf[StructType]

    val jsonStruct = parse(
      """{
        | "city": "City",
        | "houseNumber": 123,
        | "street": "Street",
        | "zip": "ZIP",
        | "fieldNotInType": true
        |}""".stripMargin
    ).getOrElse(Json.Null)

    val value: Map[String, Any] = dataType.valueWithContext(jsonStruct, DataType.baseDataTypes).getOrElse(Map())
    value.get("city") shouldBe Some("City")
    value.get("houseNumber") shouldBe Some(123)
    value.get("street") shouldBe Some("Street")
    value.get("zip") shouldBe Some("ZIP")
    value.get("fieldNotInType") shouldBe None

    val jsonCollection = parse(
      """[{
        | "city": "City",
        | "houseNumber": 123,
        | "street": "Street",
        | "zip": "ZIP"
        |}]""".stripMargin
    ).getOrElse(Json.Null)

    val result2 = Feature.dataType(addressesArtifact)
    result2.isSuccess shouldBe true

    val collectionDataType = result2.get.asInstanceOf[CollectionType]

    val value2 = collectionDataType.valueWithContext(jsonCollection, DataType.baseDataTypes + (dataType.id -> dataType)).getOrElse(Seq())
    value2.head.asInstanceOf[Map[String,Any]].keySet shouldBe value.keySet
  }

  it should "fail converting JSON with different data types" in {

    val result = Feature.dataType(addressArtifact)
    result.isSuccess shouldBe true

    val dataType: StructType = result.get.asInstanceOf[StructType]

    val json = parse(
      """{
        | "city": "City",
        | "houseNumber": "123",
        | "street": "Street",
        | "zip": "ZIP"
        |}""".stripMargin
    ).getOrElse(Json.Null)

    dataType.valueWithContext(json, DataType.baseDataTypes) shouldBe Failure(InvalidTypeForField("houseNumber", InvalidType("Integer")))

    dataType.value(Json.Null) shouldBe Failure(InvalidType(dataType.name))

    val address = Feature.dataType(addressArtifact).get
    val context = Map(address.id -> address)
    Feature.dataType(addressesArtifact).flatMap(_.valueWithContext(json, context)) shouldBe Failure(InvalidType("Addresses"))
    Feature.dataType(addressesArtifact).flatMap(_.value(json)) shouldBe Failure(UnknownType(address.id))

    val personJson = parse(
      """{
        | "fistName": "Max",
        | "lastName": "Mustermann",
        | "age": 40,
        | "adresses": []
        |}""".stripMargin
    ).getOrElse(Json.Null)
    val addresses = Feature.dataType(addressesArtifact).get
    person.valueWithContext(personJson, DataType.baseDataTypes + (person.id -> person)) shouldBe Failure(UnknownType(addresses.id))

    char.valueWithContext(Json.fromString("A"), Map()) shouldBe Failure(UnknownType(StringType.id))
  }


  it should "convert JSON for type Any" in {

    val dataType = StructType("", "AnyStruct", None, Map(
      "int" -> Field("int", AnyType.id, false),
      "bool" -> Field("bool", AnyType.id, false),
      "str" -> Field("str", AnyType.id, false),
      "obj" -> Field("obj", AnyType.id, false),
      "arr" -> Field("arr", AnyType.id, false)
    ))

    val json = parse(
      """{
        | "int": 123,
        | "bool": true,
        | "str": "String",
        | "obj": { "f": true },
        | "arr": [1,2,3]
        |}""".stripMargin
    ).getOrElse(Json.Null)

    val result = dataType.valueWithContext(json, DataType.baseDataTypes)
    result.isSuccess shouldBe true
    val struct = result.get
    struct.keySet shouldBe Set("int", "bool", "str", "obj", "arr")
    struct("int") shouldBe 123
    struct("bool") shouldBe true
    struct("str") shouldBe "String"
    struct("obj").isInstanceOf[Map[_,_]] shouldBe true
    val inner = struct("obj").asInstanceOf[Map[String,Any]]
    inner.keySet shouldBe Set("f")
    inner("f") shouldBe true
    struct("arr").isInstanceOf[Seq[_]] shouldBe true
    struct("arr").asInstanceOf[Seq[Long]] shouldBe Seq(1,2,3)


  }

  it should "convert regarding Min and Max constraints" in {

    char.value(Json.fromString("A")) shouldBe Success("A")
    char.value(Json.fromString("AB")) shouldBe Failure(ConstraintViolation(Max(1)))
    char.value(Json.fromString("")) shouldBe Failure(ConstraintViolation(Min(1)))

    modulo11.value(Json.fromInt(0)) shouldBe Success(0)
    modulo11.value(Json.fromInt(11)) shouldBe Failure(ConstraintViolation(Max(10)))
    modulo11.value(Json.fromInt(-1)) shouldBe Failure(ConstraintViolation(Min(0)))

  }

  it should "convert regarding FromDate and DueDate constraints" in {

    val fromDate = LocalDateTime.parse("2021-12-25T00:00:00", formatter)
    val dueDate = LocalDateTime.parse("2021-12-26T23:59:00", formatter)

    christmas21.value(Json.fromString("2021-12-25T08:00:00")) shouldBe Success("2021-12-25T08:00:00")
    christmas21.value(Json.fromString("2021-12-24T08:00:00")) shouldBe Failure(ConstraintViolation(FromDate(fromDate)))
    christmas21.value(Json.fromString("2021-12-27T08:00:00")) shouldBe Failure(ConstraintViolation(DueDate(dueDate)))

  }

  it should "convert regarding PossibleValues constraint" in {

    rgb.value(Json.fromString("Red")) shouldBe Success("Red")
    rgb.value(Json.fromString("Yellow")) shouldBe Failure(ConstraintViolation(PossibleValues(Set("Red", "Green", "Blue"))))

  }

  it should "convert regarding Regex constraint" in {

    digit.value(Json.fromString("3")) shouldBe Success("3")
    digit.value(Json.fromString("12")) shouldBe Failure(ConstraintViolation(Regex("""^\d$""")))
    digit.value(Json.fromString("a")) shouldBe Failure(ConstraintViolation(Regex("""^\d$""")))


  }

}
