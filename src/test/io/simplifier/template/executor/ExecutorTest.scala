package io.simplifier.template.executor

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class ExecutorTest extends TestKit(ActorSystem("ExecutorTest", ConfigFactory.empty))
  with AnyFlatSpecLike
  with BeforeAndAfterAll
  with Matchers
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  "An Executor" should "create a Renderer and forward incomming requests" in new BaseFixture {

    executor ! RenderTemplate("name", "folder", Json.Null, "test")

    renderer.expectMsgPF(30.seconds) {
      case RenderTemplate("name", "folder", _, "test") => "ok"
    }
  }

  trait BaseFixture {

    val provider = TestProbe("Provider")
    val renderer = TestProbe("StorageProvider")
    val rendererProps = {
      class Wrapper extends Actor {
        override def receive: Receive = {
          case x => renderer.ref forward x
        }
      }
      Props(new Wrapper)
    }

    val executor = system.actorOf(Executor.props(provider.ref, rendererProps))
  }
}
