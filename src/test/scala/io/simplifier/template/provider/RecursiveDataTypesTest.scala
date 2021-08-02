package io.simplifier.template.provider

import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.KeepRunning
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.gilt.handlebars.scala.binding.dynamic.valueToBinding
import com.typesafe.config.ConfigFactory
import io.circe.Json
import io.simplifier.template.provider.DataTypeLoader.DynamicDataTypeLoader
import io.simplifier.template.provider.Provider.{DataTypeById, DataTypeWithDependencies}
import io.simplifier.template.provider.StorageProvider.{ReadString, StringRead}
import org.apache.commons.io.IOUtils
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import io.circe.parser._

import scala.concurrent.duration.DurationInt

class RecursiveDataTypesTest extends TestKit(ActorSystem("RecursiveDataTypesTest", ConfigFactory.empty))
  with AnyFlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  val list = IOUtils.resourceToString("/list.json", StandardCharsets.UTF_8)
  val tree = IOUtils.resourceToString("/tree.json", StandardCharsets.UTF_8)
  val trees = IOUtils.resourceToString("/trees.json", StandardCharsets.UTF_8)

  val fs = Map(
    "1" -> list,
    "2" -> tree,
    "3" -> trees
  )


  "A DataTypeProvider" should "provide dirctly recursive types" in new BaseFixture {

    provider ! DataTypeById("1")

    expectMsgPF(30.seconds) {
      case DataTypeWithDependencies(dataType, dependencies) =>
        dataType.name shouldBe "List"

        val json = parse(
          """{
            |  "value": 1,
            |  "next": {
            |    "value": 2,
            |    "next": {
            |      "value": 3
            |    }
            |  }
            |}""".stripMargin).getOrElse(Json.Null)

        val value = dataType.valueWithContext(json, dependencies)
        value.isSuccess shouldBe true
        val valueMap = value.get.asInstanceOf[Map[String, Any]]
        valueMap.get("value") shouldBe Some(1)
        valueMap.get("next").isInstanceOf[Some[_]] shouldBe true
        val next = valueMap("next").asInstanceOf[Map[String,Any]]
        next.get("value") shouldBe Some(2)
    }

  }

  it should "provide indirctly recursive types" in new BaseFixture {

    provider ! DataTypeById("2")

    expectMsgPF(30.seconds) {
      case DataTypeWithDependencies(dataType, dependencies) =>
        dataType.name shouldBe "Tree"

        val json = parse(
          """{
            |  "value": 1,
            |  "trees": [
            |    {
            |      "value": 2
            |    },
            |    {
            |      "value": 3,
            |      "trees": [
            |        {
            |          "value": 4,
            |          "trees": []
            |        }
            |      ]
            |    }
            |  ]
            |}""".stripMargin).getOrElse(Json.Null)

        val value = dataType.valueWithContext(json, dependencies)
        value.isSuccess shouldBe true
        val valueMap = value.get.asInstanceOf[Map[String, Any]]
        valueMap.get("value") shouldBe Some(1)
        valueMap.get("trees").isInstanceOf[Some[_]] shouldBe true
        val level1 = valueMap("trees").asInstanceOf[Vector[Map[String,Any]]]
        level1.size shouldBe 2
        level1.head("value") shouldBe 2
    }

  }

  trait BaseFixture {

    val storageProvider = TestProbe("StorageProvider")

    storageProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ReadString(id) =>
        sender ! StringRead(id, fs(id))
        KeepRunning
      case x =>
        KeepRunning
    })
    val providerProps = {
      class Wrapper extends Actor {
        override def receive: Receive = {
          case x =>
            storageProvider.ref forward x
        }
      }
      Props(new Wrapper)
    }

    val dynamicLoader: DynamicDataTypeLoader =
      (dataTypeId: String, provider: ActorRef) => DataTypeLoader.props(dataTypeId, provider, providerProps)

    val provider = system.actorOf(DataTypeProvider.props(dynamicLoader))
  }

}
