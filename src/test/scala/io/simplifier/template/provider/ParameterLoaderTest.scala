package io.simplifier.template.provider

import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.KeepRunning
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import io.simplifier.template.design.transport.Feature
import io.simplifier.template.entity.DataType
import io.simplifier.template.provider.DataTypeLoader.CompleteDataType
import io.simplifier.template.provider.DataTypeProvider.DataTypeError
import io.simplifier.template.provider.ParameterLoader.{ParametersError, ParametersNotFound, ParametersStorageError}
import io.simplifier.template.provider.Provider.{DataTypeById, DataTypeWithDependencies, ParametersForTemplate, ParametersWithDataTypes}
import io.simplifier.template.provider.StorageProvider.{NotFound, ReadString, StorageError, StringRead}
import org.apache.commons.io.IOUtils
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class ParameterLoaderTest extends TestKit(ActorSystem("ParameterLoaderTest", ConfigFactory.empty()))
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
  val dataType = Feature.dataType(person).get

  "A ParameterLoader" should "load parameters from storage" in new BaseFixture {

    val filePath = ParameterLoader.parametersName("name", "folder")

    storageProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ReadString(`filePath`) =>
        sender ! StringRead(filePath, artifact)
        KeepRunning
    })

    dataTypeProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case DataTypeById("937F96E7F9FE42E3B9395B5E188013F225264BEE30B90B4D7A4EE3681817E416", _) =>
        sender ! DataTypeWithDependencies(dataType, DataType.baseDataTypes + (dataType.id -> dataType))
        KeepRunning
    })

    loader ! ParametersForTemplate("name", "folder")

    expectMsgPF(30.seconds) {
      case ParametersWithDataTypes(_, _, true, parameters, dataTypes) =>
        parameters should have size 1
        parameters.head.name shouldBe "dude"
        parameters.head.alias shouldBe Some("person")
        parameters.head.dataTypeId shouldBe "937F96E7F9FE42E3B9395B5E188013F225264BEE30B90B4D7A4EE3681817E416"
        dataTypes.contains(parameters.head.dataTypeId) shouldBe true
        dataTypes should have size 7
    }

  }

  it should "fail in case of a storage error" in new BaseFixture {

    val filePath = ParameterLoader.parametersName("name", "folder")

    storageProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ReadString(`filePath`) =>
        sender ! StorageError(filePath, new RuntimeException("Uh-Oh!"))
        KeepRunning
    })

    loader ! ParametersForTemplate("name", "folder")

    expectMsgPF(30.seconds) {
      case ParametersStorageError("name", "folder", StorageError(path, _)) =>
        path shouldBe filePath
    }

  }

  it should "fail in case parameters don't exist" in new BaseFixture {

    val filePath = ParameterLoader.parametersName("name", "folder")

    storageProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ReadString(`filePath`) =>
        sender ! NotFound(filePath)
        KeepRunning
    })

    loader ! ParametersForTemplate("name", "folder")

    expectMsgPF(30.seconds) {
      case ParametersNotFound("name", "folder") => "ok"
    }

  }

  it should "fail in case of a data type error" in new BaseFixture {

    val filePath = ParameterLoader.parametersName("name", "folder")

    storageProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ReadString(`filePath`) =>
        sender ! StringRead(filePath, artifact)
        KeepRunning
    })

    dataTypeProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case DataTypeById(id@"937F96E7F9FE42E3B9395B5E188013F225264BEE30B90B4D7A4EE3681817E416", _) =>
        sender ! DataTypeError(id, new RuntimeException("Uh-Oh!"))
        KeepRunning
    })

    loader ! ParametersForTemplate("name", "folder")

    expectMsgPF(30.seconds) {
      case ParametersStorageError("name", "folder", DataTypeError(id, _)) =>
        id shouldBe dataType.id
    }

  }

  trait BaseFixture {
    val dataTypeProvider = TestProbe("DataTypeProvider")
    val storageProvider = TestProbe("StorageProvider")
    val providerProps = {
      class Wrapper extends Actor {
        override def receive: Receive = {
          case x => storageProvider.ref forward x
        }
      }
      Props(new Wrapper)
    }
    val loader = system.actorOf(ParameterLoader.props("name", "folder", dataTypeProvider.ref, providerProps))
  }

}
