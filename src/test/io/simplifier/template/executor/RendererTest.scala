package io.simplifier.template.executor

import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActor, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import io.circe.parser._

import scala.concurrent.duration._

class RendererTest extends TestKit(ActorSystem("RendererTest", ConfigFactory.empty()))
  with AnyFlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  "A Renderer" should "render a template" in new BaseFixture {

    val name = "name"
    val folder = "folder"

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case TemplateForName(`name`, `folder`) =>
        sender ! Template(name, folder,
          s"""{{#dude}}Hello {{firstName}}! How is the weather in {{address.city}}?{{/dude}}""")
        TestActor.KeepRunning
    })

    parameterConverter.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case InputForTemplate(`name`, `folder`, _) =>
        sender ! ParametersForRendering(name, folder, Map(
          "dude" -> Map(
            "firstName" -> "Simple",
            "lastName" -> "Huber",
            "address" -> Map(
              "city" -> "Würzburg"
            )
          )))
        TestActor.KeepRunning
    })

    renderer ! RenderTemplate(name, folder, parse(
      """{}""".stripMargin
    ).getOrElse(Json.Null), "")

    expectMsgPF(TIMEOUT) {
      case RenderedTemplate(item, "") =>
        item shouldBe "Hello Simple! How is the weather in Würzburg?"
    }

  }

  it should "send an error when parameter validation failed" in new BaseFixture {

    val name = "name"
    val folder = "folder"

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case TemplateForName(`name`, `folder`) =>
        sender ! Template(name, folder,
          s"""{{#dude}}Hello {{firstName}}! How is the weather in {{address.city}}?{{/dude}}""")
        TestActor.KeepRunning
    })

    parameterConverter.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case InputForTemplate(`name`, `folder`, _) =>
        sender ! ParametersValidationError(name, folder,
          InvalidType("Integer"))
        TestActor.KeepRunning
    })

    renderer ! RenderTemplate(name, folder, parse(
      """{}""".stripMargin
    ).getOrElse(Json.Null), "")

    expectMsgPF(TIMEOUT) {
      case RenderValidationError(`name`, `folder`, e, "") =>
        e shouldBe ParametersValidationError(name, folder, InvalidType("Integer"))
    }

  }

  it should "send an error when parameter retreiving failed" in new BaseFixture {

    val name = "name"
    val folder = "folder"

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case TemplateForName(`name`, `folder`) =>
        sender ! Template(name, folder,
          s"""{{#dude}}Hello {{firstName}}! How is the weather in {{address.city}}?{{/dude}}""")
        TestActor.KeepRunning
    })

    parameterConverter.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case InputForTemplate(`name`, `folder`, _) =>
        sender ! ParametersRetreivingError(name, folder,
          ParametersStorageError(name, folder, new RuntimeException("Uh-Oh!")))
        TestActor.KeepRunning
    })

    renderer ! RenderTemplate(name, folder, parse(
      """{}""".stripMargin
    ).getOrElse(Json.Null), "")

    expectMsgPF(TIMEOUT) {
      case RenderStorageError(`name`, `folder`, e, "") =>
        e should matchPattern { case ParametersRetreivingError(`name`, `folder`, ParametersStorageError(`name`, `folder`, _)) => }
    }

  }

  it should "send an error when the template could not be loaded" in new BaseFixture {

    val name = "name"
    val folder = "folder"

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case TemplateForName(`name`, `folder`) =>
        sender ! TemplateStorageError(name, folder, new RuntimeException("Uh-Oh!"))
        TestActor.KeepRunning
    })

    parameterConverter.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case InputForTemplate(`name`, `folder`, _) =>
        sender ! ParametersForRendering(name, folder, Map(
          "dude" -> Map(
            "firstName" -> "Simple",
            "lastName" -> "Huber",
            "address" -> Map(
              "city" -> "Würzburg"
            )
          )))
        TestActor.KeepRunning
    })

    renderer ! RenderTemplate(name, folder, parse(
      """{}""".stripMargin
    ).getOrElse(Json.Null), "")

    expectMsgPF(TIMEOUT) {
      case RenderStorageError(`name`, `folder`, TemplateStorageError(`name`, `folder`, _), "") => "ok"
    }

  }

  it should "send an error when the template doesn't exist" in new BaseFixture {

    val name = "name"
    val folder = "folder"

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case TemplateForName(`name`, `folder`) =>
        sender ! TemplateNotFound(name, folder, new RuntimeException("Uh-Oh!"))
        TestActor.KeepRunning
    })

    parameterConverter.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case InputForTemplate(`name`, `folder`, _) =>
        sender ! ParametersForRendering(name, folder, Map(
          "dude" -> Map(
            "firstName" -> "Simple",
            "lastName" -> "Huber",
            "address" -> Map(
              "city" -> "Würzburg"
            )
          )))
        TestActor.KeepRunning
    })

    renderer ! RenderTemplate(name, folder, parse(
      """{}""".stripMargin
    ).getOrElse(Json.Null), "")

    expectMsgPF(TIMEOUT) {
      case RenderNotFound(`name`, `folder`, TemplateNotFound(`name`, `folder`, _), "") => "ok"
    }

  }

  it should "send an error when the parameters don't exist" in new BaseFixture {

    val name = "name"
    val folder = "folder"

    provider.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case TemplateForName(`name`, `folder`) =>
        sender ! Template(name, folder,
          s"""{{#dude}}Hello {{firstName}}! How is the weather in {{address.city}}?{{/dude}}""")
        TestActor.KeepRunning
    })

    parameterConverter.setAutoPilot((sender: ActorRef, msg: Any) => msg match {
      case InputForTemplate(`name`, `folder`, _) =>
        sender ! ParametersRetreivingError(name, folder, ParametersNotFound(name, folder))
        TestActor.KeepRunning
    })

    renderer ! RenderTemplate(name, folder, parse(
      """{}""".stripMargin
    ).getOrElse(Json.Null), "")

    expectMsgPF(TIMEOUT) {
      case RenderNotFound(`name`, `folder`, ParametersRetreivingError(`name`, `folder`, _), "") => "ok"
    }

  }

  trait BaseFixture {

    val TIMEOUT = 30.seconds

    val provider = TestProbe("Provider")
    val parameterConverter = TestProbe("ParameterConverter")
    val converterProps = {
      class Wrapper extends Actor {
        override def receive: Receive = {
          case x => parameterConverter.ref forward x
        }
      }
      Props(new Wrapper)
    }

    val renderer = system.actorOf(Renderer.props(provider.ref, converterProps))

  }

}
