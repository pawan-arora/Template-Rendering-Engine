package io.simplifier.template.executor

import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.KeepRunning
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import io.circe.parser._
import io.simplifier.template.design.transport.Feature
import io.simplifier.template.entity.DataType.{InvalidType, InvalidTypeForField, StringType}
import io.simplifier.template.entity.{DataType, Parameter}
import io.simplifier.template.executor.ParameterConverter.{InputForTemplate, InvalidParameter, MissingParameter, ParametersForRendering, ParametersRetreivingError, ParametersValidationError}
import io.simplifier.template.provider.ParameterLoader.{ParametersError, ParametersNotFound, ParametersStorageError}
import io.simplifier.template.provider.ParameterProvider
import io.simplifier.template.provider.Provider.{ParametersForTemplate, ParametersWithDataTypes}
import org.apache.commons.io.IOUtils
import org.w3c.dom.ranges.RangeException

import scala.concurrent.duration._

class ParameterConverterTest extends TestKit(ActorSystem("ParemeterConverterTest", ConfigFactory.empty()))
  with AnyFlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  val templateF = IOUtils.resourceToString("/templateArtifact.json", StandardCharsets.UTF_8)
  val personF = IOUtils.resourceToString("/person.json", StandardCharsets.UTF_8)
  val addressesF = IOUtils.resourceToString("/addresses.json", StandardCharsets.UTF_8)
  val addressF = IOUtils.resourceToString("/address.json", StandardCharsets.UTF_8)
  val parameters = Feature.parameters(templateF).get
  val person = Feature.dataType(personF).get
  val addresses = Feature.dataType(addressesF).get
  val address = Feature.dataType(addressF).get

  "A ParameterConverter" should "convert JSON-Objects without validation" in new BaseFixture {

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersWithDataTypes("name", "folder", false, parameters.parameters, Map(
          person.id -> person,
          addresses.id -> addresses,
          address.id -> address
        ) ++ DataType.baseDataTypes)
        KeepRunning
    })

    converter ! InputForTemplate("name", "folder", parse(
      """{
        |"person": {
        |    "firstName": "Simple",
        |    "lastName": "Huber",
        |    "height": 1.125,
        |    "intelligence": null,
        |    "addresses": [
        |      {
        |        "street": "SomeStreet",
        |        "city": "SomeCity",
        |        "ZIP": 1234,
        |        "home": true
        |      }
        |    ]
        |  }
        |}""".stripMargin).getOrElse(Json.Null))

    expectMsgPF(30.seconds) {
      case ParametersForRendering("name", "folder", someMap) =>
        val dude = someMap("dude").asInstanceOf[Map[String, Any]]
        dude("firstName") shouldBe "Simple"
        dude("lastName") shouldBe "Huber"
        dude("height") shouldBe a[Double]
        dude("height") shouldBe 1.125
        Option(dude("intelligence")) shouldBe None
        dude("addresses").asInstanceOf[Vector[Map[String, Any]]] should have size 1
        val address = dude("addresses").asInstanceOf[Iterable[Map[String, Any]]].head
        address("street") shouldBe "SomeStreet"
        address("city") shouldBe "SomeCity"
        address("ZIP") shouldBe a[Long]
        address("ZIP") shouldBe 1234
    }
  }

  it should "convert a Json Object based on typed parameters" in new BaseFixture {

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersWithDataTypes("name", "folder", parameters.validate, parameters.parameters, Map(
          person.id -> person,
          addresses.id -> addresses,
          address.id -> address
        ) ++ DataType.baseDataTypes)
        KeepRunning
    })

    converter ! InputForTemplate("name", "folder", parse(
      """{
        |"person": {
        |    "firstName": "Simple",
        |    "lastName": "Huber",
        |    "height": 1.125,
        |    "intelligence": null,
        |    "adresses": [
        |      {
        |        "street": "SomeStreet",
        |        "city": "SomeCity",
        |        "zip": "1234",
        |        "home": true
        |      }
        |    ]
        |  }
        |}""".stripMargin).getOrElse(Json.Null))

    expectMsgPF(30.seconds) {
      case ParametersForRendering("name", "folder", someMap) =>
        val dude = someMap("dude").asInstanceOf[Map[String, Any]]
        dude("firstName") shouldBe "Simple"
        dude("lastName") shouldBe "Huber"
        dude.get("height") shouldBe None
        dude.get("intelligence") shouldBe None
        dude("adresses").asInstanceOf[Vector[Map[String, Any]]] should have size 1
        val address = dude("adresses").asInstanceOf[Iterable[Map[String, Any]]].head
        address("street") shouldBe "SomeStreet"
        address("city") shouldBe "SomeCity"
        address("zip") shouldBe "1234"
    }

  }

  it should "add const values to input JSON on typeless conversion" in new BaseFixture {

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersWithDataTypes("name", "folder", false, Seq(
          Parameter("name", None, Some("World"), StringType.id, true, None)
        ), DataType.baseDataTypes)
        KeepRunning
    })

    converter ! InputForTemplate("name", "folder", parse(
      """{
        |"name": "Input"
        |}""".stripMargin).getOrElse(Json.Null))

    expectMsgPF(30.seconds) {
      case ParametersForRendering("name", "folder", someMap) =>
        someMap("name") shouldBe "World"
    }

  }

  it should "add const values to input JSON on typed conversion" in new BaseFixture {

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersWithDataTypes("name", "folder", true, Seq(
          Parameter("name", None, Some("World"), StringType.id, true, None)
        ), DataType.baseDataTypes)
        KeepRunning
    })

    converter ! InputForTemplate("name", "folder", parse(
      """{
        |"name": "Input"
        |}""".stripMargin).getOrElse(Json.Null))

    expectMsgPF(30.seconds) {
      case ParametersForRendering("name", "folder", someMap) =>
        someMap("name") shouldBe "World"
    }

  }

  it should "send an error when the input is not type conform" in new BaseFixture {

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersWithDataTypes("name", "folder", parameters.validate, parameters.parameters, Map(
          person.id -> person,
          addresses.id -> addresses,
          address.id -> address
        ) ++ DataType.baseDataTypes)
        KeepRunning
    })

    converter ! InputForTemplate("name", "folder", parse(
      """{
        |"person": {
        |    "firstName": "Simple",
        |    "lastName": true,
        |    "height": 1.125,
        |    "intelligence": null,
        |    "adresses": [
        |      {
        |        "street": "SomeStreet",
        |        "city": "SomeCity",
        |        "zip": "1234",
        |        "home": true
        |      }
        |    ]
        |  }
        |}""".stripMargin).getOrElse(Json.Null))

    expectMsgPF(30.seconds) {
      case ParametersValidationError("name", "folder", reason) =>
        reason shouldBe InvalidParameter("dude", InvalidTypeForField("lastName", InvalidType("String")))
    }

  }

  it should "send an error when a parameters error occured" in new BaseFixture {

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersStorageError("name", "folder", new RuntimeException("Uh-Oh!"))
        KeepRunning
    })

    converter ! InputForTemplate("name", "folder", parse(
      """{
        |"person": {
        |    "firstName": "Simple",
        |    "lastName": true,
        |    "height": 1.125,
        |    "intelligence": null,
        |    "adresses": [
        |      {
        |        "street": "SomeStreet",
        |        "city": "SomeCity",
        |        "zip": "1234",
        |        "home": true
        |      }
        |    ]
        |  }
        |}""".stripMargin).getOrElse(Json.Null))

    expectMsgPF(30.seconds) {
      case ParametersRetreivingError("name", "folder", reason) =>
        reason should matchPattern { case ParametersStorageError("name", "folder", _) => }
    }

  }

  it should "send an error when parameters don't exist" in new BaseFixture {

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersNotFound("name", "folder")
        KeepRunning
    })

    converter ! InputForTemplate("name", "folder", parse(
      """{
        |"person": {
        |    "firstName": "Simple",
        |    "lastName": true,
        |    "height": 1.125,
        |    "intelligence": null,
        |    "adresses": [
        |      {
        |        "street": "SomeStreet",
        |        "city": "SomeCity",
        |        "zip": "1234",
        |        "home": true
        |      }
        |    ]
        |  }
        |}""".stripMargin).getOrElse(Json.Null))

    expectMsgPF(30.seconds) {
      case ParametersRetreivingError("name", "folder", reason) =>
        reason should matchPattern { case ParametersNotFound("name", "folder") => }
    }

  }

  it should "send an error if parameter is missing or typed wrongly" in new BaseFixture {

    val nonOptionalParameters = parameters.parameters.map(p => p.copy(optional = false))

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersWithDataTypes("name", "folder", parameters.validate, nonOptionalParameters, Map(
          person.id -> person,
          addresses.id -> addresses,
          address.id -> address
        ) ++ DataType.baseDataTypes)
        KeepRunning
    })

    converter ! InputForTemplate("name", "folder", parse(
      """{
        |"Person": {
        |    "firstName": "Simple",
        |    "lastName": "Huber",
        |    "height": 1.125,
        |    "intelligence": null,
        |    "adresses": [
        |      {
        |        "street": "SomeStreet",
        |        "city": "SomeCity",
        |        "zip": "1234",
        |        "home": true
        |      }
        |    ]
        |  }
        |}""".stripMargin).getOrElse(Json.Null))

    expectMsgPF(30.seconds) {
      case ParametersValidationError("name", "folder", MissingParameter(Parameter("dude", Some("person"), _, _, _, _))) => "ok"
    }

  }

  trait BaseFixture {

    val provider = TestProbe("Provider")

    val converter: ActorRef = system.actorOf(ParameterConverter.props(provider.ref))

  }

}
