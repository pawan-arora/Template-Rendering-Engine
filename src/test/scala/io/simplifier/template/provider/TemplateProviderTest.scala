package io.simplifier.template.provider

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestActor.KeepRunning
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import io.simplifier.template.provider.PersistenceAccess.ItemNotFound
import io.simplifier.template.provider.Provider.{Template, TemplateForName}
import io.simplifier.template.provider.StorageProvider.{NotFound, ReadString, StorageError, StringRead}
import io.simplifier.template.provider.TemplateProvider.{TemplateError, TemplateNotFound, TemplateStorageError}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class TemplateProviderTest extends TestKit(ActorSystem("TemplateProviderTest", ConfigFactory.empty))
  with AnyFlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  "A TemplateProvider" should "provide an uncached Template" in new BaseFixture {

    val filePath = TemplateProvider.templateName("name", "folder")

    storageProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ReadString(`filePath`) =>
        sender ! StringRead("folder/name/template", "template")
        KeepRunning
    })

    provider ! TemplateForName("name", "folder")

    expectMsgPF(30.seconds) {
      case Template(_, _, content) =>
        content shouldBe "template"
    }

  }

  it should "fail in case of a storage error" in new BaseFixture {

    val filePath = TemplateProvider.templateName("name", "folder")

    storageProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ReadString(`filePath`) =>
        sender ! StorageError(filePath, new RuntimeException("Uh-Oh!"))
        KeepRunning
    })

    provider ! TemplateForName("name", "folder")

    expectMsgPF(30.seconds) {
      case TemplateStorageError("name", "folder", StorageError(path, _)) =>
        path shouldBe filePath
    }

  }

  it should "fail in case of the template doesn't exist" in new BaseFixture {

    val filePath = TemplateProvider.templateName("name", "folder")

    storageProvider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case ReadString(`filePath`) =>
        sender ! NotFound(filePath)
        KeepRunning
    })

    provider ! TemplateForName("name", "folder")

    expectMsgPF(30.seconds) {
      case TemplateNotFound("name", "folder", NotFound(path)) =>
        path shouldBe filePath
    }

  }

  trait BaseFixture {

    val storageProvider = TestProbe("StorageProvider")
    val providerProps = {
      class Wrapper extends Actor {
        override def receive: Receive = {
          case x => storageProvider.ref forward x
        }
      }
      Props(new Wrapper)
    }
    val provider = system.actorOf(TemplateProvider.props(providerProps))
  }

}
