package io.simplifier.template.provider

import java.nio.charset.StandardCharsets

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import io.simplifier.template.provider.PersistenceAccess.ItemNotFound
import io.simplifier.template.provider.StorageProvider._
import org.mockito.MockitoSugar._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util._
import scala.concurrent.duration._

class StorageProviderTest extends TestKit(ActorSystem("StorageProvider", ConfigFactory.empty()))
  with AnyFlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  "A StorageProvider" should "provide a string content" in new BaseFixture {

    when(access.readString("folder/existing/template")).thenReturn(Success("template"))

    provider ! ReadString("folder/existing/template")

    expectMsgPF(30.seconds) {
      case StringRead(name, content) =>
        name shouldBe "folder/existing/template"
        content shouldBe "template"
    }

  }

  it should "provide a byte array content" in new BaseFixture {

    when(access.readBytes("folder/existing/template")).thenReturn(Success("template".getBytes))

    provider ! ReadBytes("folder/existing/template")

    expectMsgPF(30.seconds) {
      case BytesRead(name, content) =>
        name shouldBe "folder/existing/template"
        new String(content, StandardCharsets.UTF_8) shouldBe "template"
    }

  }

  it should "send a storage error on failing string read" in new BaseFixture {

    when(access.readString("folder/existing/template")).thenReturn(Failure(new RuntimeException("Uh-Oh!")))

    provider ! ReadString("folder/existing/template")

    expectMsgPF(30.seconds) {
      case StorageError(name, exception) =>
        name shouldBe "folder/existing/template"
        exception.getMessage shouldBe "Uh-Oh!"
    }

  }

  it should "send a storage error on failing bytes read" in new BaseFixture {

    when(access.readBytes("folder/existing/template")).thenReturn(Failure(new RuntimeException("Uh-Oh!")))

    provider ! ReadBytes("folder/existing/template")

    expectMsgPF(30.seconds) {
      case StorageError(name, exception) =>
        name shouldBe "folder/existing/template"
        exception.getMessage shouldBe "Uh-Oh!"
    }

  }

  it should "send a NotFound if item doesn't exist" in new BaseFixture {

    val name = "folder/notexsiting/template"
    when(access.readString(name)).thenReturn(Failure(ItemNotFound(name)))
    when(access.readBytes(name)).thenReturn(Failure(ItemNotFound(name)))

    provider ! ReadString(name)

    expectMsgPF(30.seconds) {
      case NotFound(`name`) => "ok"
    }

    provider ! ReadBytes(name)

    expectMsgPF(30.seconds) {
      case NotFound(`name`) => "ok"
    }

  }

  trait BaseFixture {

    val access = mock[PersistenceAccess]

    val provider = system.actorOf(Props(new StorageProvider(access)))

  }

}
