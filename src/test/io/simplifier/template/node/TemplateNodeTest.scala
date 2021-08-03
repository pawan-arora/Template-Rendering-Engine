package io.simplifier.template.node

import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Props}
import akka.event.LoggingAdapter
import akka.http.scaladsl.model.{AttributeKey, HttpMethods, HttpProtocol, HttpProtocols, Uri}
import akka.stream.scaladsl.{Source, StreamRefs}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.simplifier.api.messages.HttpRequestBase
import io.simplifier.api.messages.v1.{HttpRequestAttributesV1, HttpRequestStreamV1, HttpRequestStrictV1}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import io.simplifier.messages._
import io.simplifier.api.registry.ApiRouteEntry.Implicits._

import scala.concurrent.duration._

class TemplateNodeTest extends TestKit(ActorSystem("TemplateNodeTest", ConfigFactory.empty))
  with AnyFlatSpecLike
  with BeforeAndAfterAll
  with Matchers
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  val testReceiver = TestProbe("TestReceiver")

  "A TemplateNode" should "start a cluster listener to listen for member activity" in {

    val apiDispatcher = TestProbe("ApiDispatcher")

    val node = system.actorOf(TemplateNode.props(apiDispatcher.ref, listenerProps, defaultRegisterPaths))

    val listeningNode = testReceiver.receiveOne(30.seconds)
    listeningNode shouldBe node
    val roles = testReceiver.receiveOne(30.seconds)
    roles shouldBe Set(MODULE_GROUP_NAME, "apiGatewway")

  }

  it should "ask for paths to register" in {

    val apiDispatcher = TestProbe("ApiDispatcher")

    val registerReceiver = TestProbe("RegisterReceiver")

    val register = new RegisterFunction {
      override def apply(paths: Seq[ApiDispatcher.HttpRegisterPath])
                        (implicit system: ActorSystem, log: LoggingAdapter, context: ActorContext): Unit = {
        registerReceiver.ref ! paths
      }
    }

    val node = system.actorOf(TemplateNode.props(apiDispatcher.ref, listenerProps, register))

    val message = apiDispatcher.receiveOne(30.seconds)

    message shouldBe AllHttpRegisterPaths

    val pathsToRegister = HttpRegisterPaths(Seq(
      HttpRegisterPath("a/b/c".split('/').toSeq)
    ))

    node ! pathsToRegister

    registerReceiver.receiveOne(30.seconds) shouldBe pathsToRegister.paths



  }

  it should "forward HTTP requests to the API dispatcher" in {

    val apiDispatcher = TestProbe("ApiDispatcher")

    val node = system.actorOf(TemplateNode.props(apiDispatcher.ref, listenerProps, defaultRegisterPaths))

    node ! HttpRequestStrictV1(
      HttpRequestAttributesV1(Seq.empty[String].asApiPath, HttpMethods.GET, Uri("hi"), Map().asInstanceOf[Map[String, String] with BaseKryo], Map().asInstanceOf[Map[AttributeKey[_],_] with BaseKryo], HttpProtocols.`HTTP/1.1`),
    "application/json", ByteString.empty, apiDispatcher.ref)

    val sourceRef = Source[ByteString](Seq(ByteString.empty)).runWith(StreamRefs.sourceRef())
    node ! HttpRequestStreamV1(
      HttpRequestAttributesV1(Seq.empty[String].asApiPath, HttpMethods.GET, Uri("hi"), Map().asInstanceOf[Map[String, String] with BaseKryo], Map().asInstanceOf[Map[AttributeKey[_],_] with BaseKryo], HttpProtocols.`HTTP/1.1`),
      "application/json", sourceRef, apiDispatcher.ref)

    val messages = apiDispatcher.receiveN(3).tail
    messages.head.isInstanceOf[HttpRequestStrictV1] shouldBe true
    messages.tail.head.isInstanceOf[HttpRequestStreamV1] shouldBe true


  }

  val listener = TestProbe("ClusterListener")
  def listenerProps(node: ActorRef, interesstingRoles: Set[String]): Props = {
    testReceiver.ref ! node
    testReceiver.ref ! interesstingRoles
    class Wrapper extends Actor {
      override def receive: Receive = {
        case x => listener.ref forward x
      }
    }
    Props(new Wrapper)
  }

}
