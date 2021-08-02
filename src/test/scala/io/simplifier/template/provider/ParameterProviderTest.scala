package io.simplifier.template.provider

import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.KeepRunning
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import io.simplifier.template.design.transport.Feature
import io.simplifier.template.provider.ParameterLoader.{DynamicParameterLoader, ParametersError, ParametersNotFound, ParametersStorageError}
import io.simplifier.template.provider.Provider.{ParametersForTemplate, ParametersWithDataTypes}
import org.apache.commons.io.IOUtils
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class ParameterProviderTest extends TestKit(ActorSystem("ParameterProvider", ConfigFactory.empty()))
  with AnyFlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  val artifact = IOUtils.resourceToString("/templateArtifact.json", StandardCharsets.UTF_8)
  val person = IOUtils.resourceToString("/person.json", StandardCharsets.UTF_8)
  val parameters = Feature.parameters(artifact).get
  val dataType = Feature.dataType(person).get

  "A ParameterProvider" should "provide the parameters of a template" in new BaseFixture {

    parameterLoader.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersWithDataTypes("name", "folder", parameters.validate, parameters.parameters, Map(dataType.id -> dataType))
        KeepRunning
    })

    provider ! ParametersForTemplate("name", "folder")

    expectMsgPF(30.seconds) {
      case ParametersWithDataTypes(name, folder, true, returnedParameters, returnedDataTypes) =>
        name shouldBe "name"
        folder shouldBe "folder"
        returnedParameters shouldBe parameters.parameters
        returnedDataTypes should have size 1
        returnedDataTypes.values.head shouldBe dataType
    }

  }

  it should "send an error in case of a parameters error" in new BaseFixture {

    parameterLoader.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersStorageError("name", "folder", new RuntimeException("Uh-Oh!"))
        KeepRunning
    })

    provider ! ParametersForTemplate("name", "folder")

    expectMsgPF(30.seconds) {
      case ParametersStorageError("name", "folder", _) => "ok"
    }

  }

  it should "send an error in case the parameters don't exsit" in new BaseFixture {

    parameterLoader.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ParametersForTemplate("name", "folder") =>
        sender ! ParametersNotFound("name", "folder")
        KeepRunning
    })

    provider ! ParametersForTemplate("name", "folder")

    expectMsgPF(30.seconds) {
      case ParametersNotFound("name", "folder") => "ok"
    }

  }

  trait BaseFixture {

    val dataTypeProvider = TestProbe("DataTypeProvider")

    val parameterLoader = TestProbe("ParameterLoader")
    val parameterLoaderProps = {
      class Wrapper extends Actor {
        override def receive: Receive = {
          case x => parameterLoader.ref forward x
        }
      }
      Props(new Wrapper)
    }

    val dynamic: DynamicParameterLoader = (_,_,_) => parameterLoaderProps

    val provider = system.actorOf(ParameterProvider.props(dataTypeProvider.ref, dynamic))


  }

}
