package io.simplifier.template.design.transport

import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.apache.commons.io.IOUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class FeatureTest extends AnyFlatSpec with Matchers {

  val addressJson = IOUtils.resourceToString("/address.json", StandardCharsets.UTF_8)
  val addressesJson = IOUtils.resourceToString("/addresses.json", StandardCharsets.UTF_8)
  val charJson = IOUtils.resourceToString("/char.json", StandardCharsets.UTF_8)
  val christmas21Json = IOUtils.resourceToString("/christmas21.json", StandardCharsets.UTF_8)
  val digitJson = IOUtils.resourceToString("/digit.json", StandardCharsets.UTF_8)
  val modulo11Json = IOUtils.resourceToString("/modulo11.json", StandardCharsets.UTF_8)
  val rgbJson = IOUtils.resourceToString("/rgb.json", StandardCharsets.UTF_8)


  "Feature" should "convert an artifacts.json into a sequence of artifacts" in {
    val artifacts = IOUtils.resourceToString("/artifacts.json", StandardCharsets.UTF_8)
    val artifactsData = Feature.importArtifactsData(artifacts)
    println(artifactsData)
  }

  it should "convert a template artifact into parameters" in {

    val a = IOUtils.resourceToString("/templateArtifact.json", StandardCharsets.UTF_8)

    val result = Feature.parameters(a)
    result.isSuccess shouldBe true
    result.get.validate shouldBe true
    val parameters = result.get.parameters
    parameters should have size 1
    val parameter = parameters.head
    parameter shouldBe Parameter("dude", Some("person"), None, "937F96E7F9FE42E3B9395B5E188013F225264BEE30B90B4D7A4EE3681817E416", true, Some(""))

  }

  it should "convert a struct datatype artifact into a struct datatype" in {

    val result = Feature.dataType(addressJson)
    result.isSuccess shouldBe true
    val address = result.get
    address should matchPattern {
      case StructType(id, "Address", None, fields) =>
    }
    val fields = address.asInstanceOf[StructType].fields
    fields should have size 4
    fields.keySet shouldBe Set("city", "houseNumber", "street", "zip")
    fields("city").dataTypeId shouldBe StringType.id
    fields("street").dataTypeId shouldBe StringType.id
    fields("zip").dataTypeId shouldBe StringType.id
    fields("houseNumber").dataTypeId shouldBe IntegerType.id

  }

  it should "convert a collection datatype artifact into a collection datatype" in {

    val addressDataTypeId = Feature.dataType(addressJson).get.id

    val result = Feature.dataType(addressesJson)
    result.isSuccess shouldBe true
    val address = result.get
    address should matchPattern {
      case CollectionType(id, "Addresses", None, elementTypeId) =>
    }
    val elementTypeId = address.asInstanceOf[CollectionType].elementTypeId
    elementTypeId shouldBe addressDataTypeId

  }

  it should "convert a domain datatype artifact into a domain datatype" in {

    val result1 = Feature.dataType(charJson)
    result1.isSuccess shouldBe true
    val char = result1.get
    char should matchPattern {
      case DomainType(id, "Char", None, superType, constraits) =>
    }
    val domainType = char.asInstanceOf[DomainType[_]]
    val superTypeId = domainType.derivedFromId
    superTypeId shouldBe StringType.id
    domainType.constraits.toSet shouldBe Set(Min(1), Max(1), Nullable)

    val result2 = Feature.dataType(christmas21Json)
    result2.isSuccess shouldBe true
    val christmas21 = result2.get
    christmas21 should matchPattern {
      case DomainType(id, "Christmas21", None, superType, constraits) =>
    }
    val christmas21Type = christmas21.asInstanceOf[DomainType[_]]
    christmas21Type.derivedFromId shouldBe DateType.id
    val formatter = DateTimeFormatter.ISO_DATE_TIME
    val expFromDate = LocalDateTime.parse("2021-12-25T00:00:00", formatter)
    val expDueDate = LocalDateTime.parse("2021-12-26T23:59:00", formatter)
    christmas21Type.constraits.toSet shouldBe Set(FromDate(expFromDate), DueDate(expDueDate))

    val result3 = Feature.dataType(digitJson)
    result3.isSuccess shouldBe true
    val digit = result3.get
    digit should matchPattern {
      case DomainType(id, "Digit", None, superType, constraits) =>
    }
    val digitType = digit.asInstanceOf[DomainType[_]]
    digitType.derivedFromId shouldBe StringType.id
    digitType.constraits.toSet shouldBe Set(Regex("""^\d$"""), Nullable)

    val result4 = Feature.dataType(modulo11Json)
    result4.isSuccess shouldBe true
    val modulo11 = result4.get
    modulo11 should matchPattern {
      case DomainType(id, "Modulo11", None, superType, constraits) =>
    }
    val modulo11Type = modulo11.asInstanceOf[DomainType[_]]
    modulo11Type.derivedFromId shouldBe IntegerType.id
    modulo11Type.constraits.toSet shouldBe Set(Min(0), Max(10))

    val result5 = Feature.dataType(rgbJson)
    result5.isSuccess shouldBe true
    val rgb = result5.get
    rgb should matchPattern {
      case DomainType(id, "RGB", None, superType, constraits) =>
    }
    val rgbType = rgb.asInstanceOf[DomainType[_]]
    rgbType.derivedFromId shouldBe StringType.id
    rgbType.constraits.toSet shouldBe Set(PossibleValues(Set("Red", "Green", "Blue")))

  }

}
