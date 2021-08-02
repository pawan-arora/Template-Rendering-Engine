package io.simplifier.template.provider

import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.KeepRunning
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import io.simplifier.template.design.transport.Feature
import io.simplifier.template.entity.DataType
import io.simplifier.template.provider.DataTypeLoader.{CompleteDataType, IncompleteDataType}
import io.simplifier.template.provider.DataTypeProvider.DataTypeError
import io.simplifier.template.provider.Provider.{DataTypeById, DataTypeWithDependencies}
import io.simplifier.template.provider.StorageProvider.{NotFound, ReadString, StorageError, StringRead}
import org.apache.commons.io.IOUtils
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class DataTypeLoaderTest extends TestKit(ActorSystem("DataTypeLoaderTest", ConfigFactory.empty))
  with AnyFlatSpecLike
  with BeforeAndAfterAll
  with Matchers
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  val personF = IOUtils.resourceToString("/person.json", StandardCharsets.UTF_8)
  val addressesF = IOUtils.resourceToString("/addresses.json", StandardCharsets.UTF_8)
  val addressF = IOUtils.resourceToString("/address.json", StandardCharsets.UTF_8)
  val person = Feature.dataType(personF).get
  val addresses = Feature.dataType(addressesF).get
  val address = Feature.dataType(addressF).get

  "A DataTypeLoader" should "load a data type with dependent data types in cache" in new BaseFixture {

    dataTypeProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case DataTypeById(depId, excludingIds) =>
        excludingIds shouldBe Set(addresses.id)
        depId shouldBe address.id
        sender ! DataTypeWithDependencies(address, DataType.baseDataTypes + (address.id -> address))
        KeepRunning
    })

    loader ! DataTypeById(addresses.id)

    storageProvider.expectMsgPF(30.seconds) {
      case ReadString(fileName) =>
        fileName shouldBe addresses.id
        storageProvider reply StringRead(fileName, addressesF)
    }

    expectMsgPF(30.seconds) {
      case CompleteDataType(dataType, dependencies) =>
        dataType shouldBe addresses
        dependencies.get(addresses.id) shouldBe Some(addresses)
        dependencies.get(address.id) shouldBe Some(address)
    }

  }

  it should "load a data type with dependent data types loaded from other loader" in new BaseFixture {

    dataTypeProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case DataTypeById(depId, excludingIds) =>
        excludingIds shouldBe Set(addresses.id)
        depId shouldBe address.id
        sender ! IncompleteDataType(address, DataType.baseDataTypes + (address.id -> address), Set(id))
        KeepRunning
    })

    loader ! DataTypeById(addresses.id)

    storageProvider.expectMsgPF(30.seconds) {
      case ReadString(fileName) =>
        fileName shouldBe addresses.id
        storageProvider reply StringRead(fileName, addressesF)
    }

    expectMsgPF(30.seconds) {
      case CompleteDataType(dataType, dependencies) =>
        dataType shouldBe addresses
        dependencies.get(addresses.id) shouldBe Some(addresses)
        dependencies.get(address.id) shouldBe Some(address)
    }

  }

  it should "fail when storage provider fails" in new BaseFixture {

    loader ! DataTypeById(id)

    storageProvider.expectMsgPF(30.seconds) {
      case ReadString(fileName) =>
        fileName shouldBe id
        storageProvider reply StorageError(fileName, new RuntimeException("Uh-Oh!"))
    }

    expectMsgPF(30.seconds) {
      case DataTypeError(someId, StorageError(fileName, _)) =>
        someId shouldBe id
        fileName shouldBe id
    }

  }

  it should "fail when data type doesn't exist" in new BaseFixture {

    loader ! DataTypeById(id)

    storageProvider.expectMsgPF(30.seconds) {
      case ReadString(fileName) =>
        fileName shouldBe id
        storageProvider reply NotFound(fileName)
    }

    expectMsgPF(30.seconds) {
      case DataTypeError(someId, NotFound(fileName)) =>
        someId shouldBe id
        fileName shouldBe id
    }

  }

  trait BaseFixture {

    val id = "id"

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

    val loader = system.actorOf(DataTypeLoader.props(id, dataTypeProvider.ref, providerProps))
  }

  it should "fail when loading a dependent data type fails" in new BaseFixture {

    dataTypeProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case DataTypeById(depId, excludingIds) =>
        excludingIds shouldBe Set(id)
        depId shouldBe address.id
        sender ! DataTypeError(depId, StorageError(depId, new RuntimeException("Uh-Oh!")))
        KeepRunning
    })

    loader ! DataTypeById(id)

    storageProvider.expectMsgPF(30.seconds) {
      case ReadString(fileName) =>
        fileName shouldBe id
        storageProvider reply StringRead(fileName, addressesF)
    }

    expectMsgPF(30.seconds) {
      case DataTypeError(someId, DataTypeError(otherId, StorageError(fileName, _))) =>
        someId shouldBe id
        otherId shouldBe address.id
        fileName shouldBe address.id
    }

  }

}
