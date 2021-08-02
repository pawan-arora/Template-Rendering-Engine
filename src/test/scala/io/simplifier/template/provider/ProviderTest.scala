package io.simplifier.template.provider

import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.KeepRunning
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import io.simplifier.template.design.transport.Feature
import io.simplifier.template.provider.Provider.{DataTypeById, DataTypeWithDependencies, ParametersForTemplate, ParametersWithDataTypes, Template, TemplateForName}
import org.apache.commons.io.IOUtils
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class ProviderTest extends TestKit(ActorSystem("ProviderTest", ConfigFactory.empty()))
  with AnyFlatSpecLike
  with BeforeAndAfterAll
  with Matchers
  with ImplicitSender {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  val artifact = IOUtils.resourceToString("/templateArtifact.json", StandardCharsets.UTF_8)
  val person = IOUtils.resourceToString("/person.json", StandardCharsets.UTF_8)
  val parameters = Feature.parameters(artifact).get
  val dataType = Feature.dataType(person).get
  val dataTypeId = dataType.id

  "A Provider" should "provide a template" in new BaseFixture {

    provider ! TemplateForName("name", "folder")

    expectMsgPF(30.seconds) {
      case Template("name", "folder", content) =>
        content shouldBe "template"
    }
  }

  it should "provide parameters to a template" in new BaseFixture {

    provider ! ParametersForTemplate("name", "folder")

    expectMsgPF(30.seconds) {
      case ParametersWithDataTypes("name", "folder", true, params, dataTypes) =>
        params shouldBe parameters.parameters
        dataTypes should have size 1
        dataTypes.values.head shouldBe dataType
    }
  }

  it should "provide a datatype with dependent datatypes" in new BaseFixture {

    provider ! DataTypeById(dataTypeId)

    expectMsgPF(30.seconds) {
      case DataTypeWithDependencies(returnedDataType, dataTypes) =>
        returnedDataType shouldBe dataType
        dataTypes.contains(dataTypeId) shouldBe true

    }
  }

  trait BaseFixture {

    val templateProvider = TestProbe("TemplateProvider")
    val parametersProvider = TestProbe("ParametersProvider")
    val dataTypeProvider = TestProbe("DataTypeProvider")

    val provider = system.actorOf(Provider.props(templateProvider.ref, parametersProvider.ref, dataTypeProvider.ref))

    templateProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case TemplateForName("name", "folder") =>
        sender ! Template("name", "folder", "template")
        KeepRunning
    })
    parametersProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersWithDataTypes("name", "folder", parameters.validate, parameters.parameters, Map(dataType.id -> dataType))
        KeepRunning
    })
    dataTypeProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case DataTypeById(`dataTypeId`, _) =>
        sender ! DataTypeWithDependencies(dataType, Map(dataType.id -> dataType))
        KeepRunning
    })
  }

}
