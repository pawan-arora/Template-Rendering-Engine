package io.simplifier.template.provider

import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.KeepRunning
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import io.simplifier.template.design.transport.Feature
import io.simplifier.template.provider.DataTypeLoader.{CompleteDataType, DynamicDataTypeLoader}
import io.simplifier.template.provider.DataTypeProvider.DataTypeError
import io.simplifier.template.provider.Provider.{DataTypeById, DataTypeWithDependencies}
import org.apache.commons.io.IOUtils
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class DataTypeProviderTest extends TestKit(ActorSystem("DataTypeProviderTest", ConfigFactory.empty()))
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

  "A DataTypeProvider" should "provide dataTypes" in new BaseFixture {

    dataTypeLoader.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case DataTypeById(id, _) if id == person.id =>
        sender ! CompleteDataType(person, Map(
          person.id -> person,
          addresses.id -> addresses,
          address.id -> address
        ))
        KeepRunning
    })

    provider ! DataTypeById(person.id)

    expectMsgPF(30.seconds) {
      case DataTypeWithDependencies(dataType, dependencies) =>
        dataType shouldBe person
        dependencies should have size 3
    }
  }

  it should "send an error failing to load a dataType" in new BaseFixture {

    dataTypeLoader.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case DataTypeById(id, _) if id == person.id =>
        sender ! DataTypeError(id, new RuntimeException("Uh-Oh!"))
        KeepRunning
    })

    provider ! DataTypeById(person.id)

    expectMsgPF(30.seconds) {
      case DataTypeError(id, _) =>
        id shouldBe person.id
    }
  }

  trait BaseFixture {

    val dataTypeLoader = TestProbe("DataTypeLoader")
    val dataTypeLoaderProps = {
      class Wrapper extends Actor {
        override def receive: Receive = {
          case x => dataTypeLoader.ref forward x
        }
      }
      Props(new Wrapper)
    }

    def dynamicProps: DynamicDataTypeLoader = (_,_) => dataTypeLoaderProps

    val provider = system.actorOf(DataTypeProvider.props(dynamicProps))
  }

}
