package io.simplifier.template.api

import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, Uri}
import akka.stream.SystemMaterializer
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import io.simplifier.api.messages.v1.{HttpRequestStreamV1, HttpRequestStrictV1}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class ApiDispatcherTest extends TestKit(ActorSystem("ApiDispatcherTest", ConfigFactory.empty))
  with AnyFlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  "An ApiDispatcher" should "dispatch Http Messages" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val dispatcher = system.actorOf(ApiDispatcher.props(serviceProps))

    dispatcher ! new HttpRequestStrictV1(Seq("a", "b", "c"), request)

    service.expectMsgPF(30.seconds) {
      case HttpRequestStrictV1(_, _, _, _) => "ok"
    }

    dispatcher ! new HttpRequestStreamV1(Seq("a", "b", "c"), request)

    service.expectMsgPF(30.seconds) {
      case HttpRequestStreamV1(_, _, _, _) => "ok"
    }
  }

  it should "dispatch request for http register paths" in new BaseFixture {

    val dispatcher = system.actorOf(ApiDispatcher.props(serviceProps))

    dispatcher ! AllHttpRegisterPaths

    service.expectMsgPF(30.seconds) {
      case AllHttpRegisterPaths => "ok"
    }
  }

  trait BaseFixture {

    val service = TestProbe("HttpService")
    val serviceProps = {
      class Wrapper extends Actor {
        override def receive: Receive = {
          case x => service.ref forward x
        }
      }
      Props(new Wrapper)
    }

    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder/name"),
      Nil,
      HttpEntity("body")
    )

  }

}
