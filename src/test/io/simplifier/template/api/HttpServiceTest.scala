package io.simplifier.template.api

import akka.actor.ActorSystem
import akka.http.javadsl.model.{RequestEntity, StatusCodes}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, RequestEntity, Uri}
import akka.stream.SystemMaterializer
import akka.stream.scaladsl.Sink
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.{ByteString, Timeout}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import io.simplifier.api.messages.v1.{HttpRequestStreamV1, HttpRequestStrictV1, HttpResultStreamV1, HttpResultStrictV1}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import io.circe.parser._

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class HttpServiceTest extends TestKit(ActorSystem("HttpServiceTest", ConfigFactory.empty))
  with AnyFlatSpecLike
  with BeforeAndAfterAll
  with Matchers
  with ImplicitSender {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  "An HttpService" should "handle HTTP render requests (strict)" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val body = """{
                 |  "person": {
                 |    "adresses": [
                 |      {
                 |        "houseNumber": 10,
                 |        "city": "Würzburg",
                 |        "zip": "97070",
                 |        "street": "Hauptstr."
                 |      }
                 |    ],
                 |    "age": 40,
                 |    "data": { "VALUE": 0 },
                 |    "firstName": "Max",
                 |    "lastName": "Mustermann"
                 |  }
                 |}""".stripMargin

    val expParameterJson = parse(body).getOrElse(Json.Null)
    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder/name"),
      Nil,
      HttpEntity(body)
    )

    val msg: HttpRequestStrictV1 = new HttpRequestStrictV1(Seq("client", "2.0", "template", "folder", "name"), request)
    httpService ! msg

    executor.expectMsgPF(30.seconds) {
      case RenderTemplate(name, folder, parameters, label) =>
        name shouldBe "name"
        folder shouldBe "folder"
        parameters shouldBe expParameterJson
        executor reply RenderedTemplate("done", label)
    }

    expectMsgPF(30.seconds) {
      case HttpResultStreamV1(attributes, content, contentType) =>
        attributes.statusCode shouldBe StatusCodes.OK.intValue
        contentType.mediaType.toString shouldBe "application/json"
        val result = Await.result(content.source.runWith(Sink.fold(ByteString.empty)(_ ++ _)), 30.seconds)
        result.utf8String shouldBe """{"Template":"done"}"""
    }

  }

  it should "handle HTTP render requests (streamed)" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val body = """{
                 |  "person": {
                 |    "adresses": [
                 |      {
                 |        "houseNumber": 10,
                 |        "city": "Würzburg",
                 |        "zip": "97070",
                 |        "street": "Hauptstr."
                 |      }
                 |    ],
                 |    "age": 40,
                 |    "data": { "VALUE": 0 },
                 |    "firstName": "Max",
                 |    "lastName": "Mustermann"
                 |  }
                 |}""".stripMargin

    val expParameterJson = parse(body).getOrElse(Json.Null)
    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder/name"),
      Nil,
      HttpEntity(body)
    )

    val streamMsg: HttpRequestStreamV1 = new HttpRequestStreamV1(Seq("client", "2.0", "template", "folder", "name"), request)
    httpService ! streamMsg

    executor.expectMsgPF(30.seconds) {
      case RenderTemplate(name, folder, parameters, label) =>
        name shouldBe "name"
        folder shouldBe "folder"
        parameters shouldBe expParameterJson
        executor reply RenderedTemplate("done", label)
    }

    expectMsgPF(30.seconds) {
      case HttpResultStreamV1(attributes, content, contentType) =>
        attributes.statusCode shouldBe StatusCodes.OK.intValue
        contentType.mediaType.toString shouldBe "application/json"
        val result = Await.result(content.source.runWith(Sink.fold(ByteString.empty)(_ ++ _)), 30.seconds)
        result.utf8String shouldBe """{"Template":"done"}"""
    }

  }

  it should "send proper error response on render error" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val body = """{}""".stripMargin

    val expParameterJson = parse(body).getOrElse(Json.Null)
    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder/name"),
      Nil,
      HttpEntity(body)
    )

    val msg: HttpRequestStrictV1 = new HttpRequestStrictV1(Seq("client", "2.0", "template", "folder", "name"), request)
    httpService ! msg

    executor.expectMsgPF(30.seconds) {
      case RenderTemplate(name, folder, parameters, label) =>
        name shouldBe "name"
        folder shouldBe "folder"
        parameters shouldBe expParameterJson
        executor reply RenderStorageError(name, folder, TemplateStorageError(name, folder, new RuntimeException("Uh-Oh!")), label)
    }

    expectMsgPF(30.seconds) {
      case HttpResultStrictV1(attributes, content, contentType) =>
        attributes.statusCode shouldBe StatusCodes.INTERNAL_SERVER_ERROR.intValue
        contentType.mediaType.toString shouldBe "text/plain"
        content.utf8String shouldBe "Error on rendering folder/name. Reason: Error reading template folder/name from storage. Reason: Uh-Oh!"
    }

  }

  it should "send proper error response on unknown template" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val body = """{}""".stripMargin

    val expParameterJson = parse(body).getOrElse(Json.Null)
    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder/name"),
      Nil,
      HttpEntity(body)
    )

    val msg: HttpRequestStrictV1 = new HttpRequestStrictV1(Seq("client", "2.0", "template", "folder", "name"), request)
    httpService ! msg

    executor.expectMsgPF(30.seconds) {
      case RenderTemplate(name, folder, parameters, label) =>
        name shouldBe "name"
        folder shouldBe "folder"
        parameters shouldBe expParameterJson
        executor reply RenderNotFound(name, folder, TemplateNotFound(name, folder, new RuntimeException("Uh-Oh!")), label)
    }

    expectMsgPF(30.seconds) {
      case HttpResultStrictV1(attributes, content, contentType) =>
        attributes.statusCode shouldBe StatusCodes.NOT_FOUND.intValue
        contentType.mediaType.toString shouldBe "text/plain"
        content.utf8String shouldBe "Template folder/name not found. Details: Template folder/name does not exists. Details: Uh-Oh!"
    }

  }

  it should "send proper error response on invalid input" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val body = """{}""".stripMargin

    val expParameterJson = parse(body).getOrElse(Json.Null)
    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder/name"),
      Nil,
      HttpEntity(body)
    )

    val msg: HttpRequestStrictV1 = new HttpRequestStrictV1(Seq("client", "2.0", "template", "folder", "name"), request)
    httpService ! msg

    executor.expectMsgPF(30.seconds) {
      case RenderTemplate(name, folder, parameters, label) =>
        name shouldBe "name"
        folder shouldBe "folder"
        parameters shouldBe expParameterJson
        executor reply RenderValidationError(name, folder, ParametersValidationError(name, folder, new RuntimeException("Uh-Oh!")), label)
    }

    expectMsgPF(30.seconds) {
      case HttpResultStrictV1(attributes, content, contentType) =>
        attributes.statusCode shouldBe StatusCodes.BAD_REQUEST.intValue
        contentType.mediaType.toString shouldBe "text/plain"
        content.utf8String shouldBe "Validation of input for Template folder/name failed. Details: Validation failed for template folder/name. Reason: Uh-Oh!"
    }

  }

  it should "fail convert bad render requests (strict)" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val body = """{
                 |  no json
                 |}""".stripMargin

    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder/name"),
      Nil,
      HttpEntity(body)
    )

    val msg: HttpRequestStrictV1 = new HttpRequestStrictV1(Seq("client", "2.0", "template", "folder", "name"), request)
    httpService ! msg

    expectMsgPF(30.seconds) {
      case HttpResultStrictV1(attributes, content, contentType) =>
        contentType.mediaType.toString shouldBe "text/plain"
        content.utf8String shouldBe ParsingError.getMessage
        attributes.statusCode shouldBe StatusCodes.BAD_REQUEST.intValue
    }

  }

  it should "fail convert bad render requests (streamed)" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val body = """{
                 |  no json
                 |}""".stripMargin

    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder/name"),
      Nil,
      HttpEntity(body)
    )

    val streamMsg: HttpRequestStreamV1 = new HttpRequestStreamV1(Seq("client", "2.0", "template", "folder", "name"), request)
    httpService ! streamMsg

    expectMsgPF(30.seconds) {
      case HttpResultStrictV1(attributes, content, contentType) =>
        contentType.mediaType.toString shouldBe "text/plain"
        content.utf8String shouldBe ParsingError.getMessage
        attributes.statusCode shouldBe StatusCodes.BAD_REQUEST.intValue
    }

  }

  it should "fail convert unsupported requests (streamed)" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder"),
      Nil,
      HttpEntity("false")
    )

    val streamMsg: HttpRequestStreamV1 = new HttpRequestStreamV1(Seq("client", "2.0", "template", "folder"), request)
    httpService ! streamMsg

    expectMsgPF(30.seconds) {
      case HttpResultStrictV1(attributes, content, contentType) =>
        contentType.mediaType.toString shouldBe "text/plain"
        content.utf8String shouldBe ServiceUnavailable.getMessage
        attributes.statusCode shouldBe StatusCodes.SERVICE_UNAVAILABLE.intValue
    }

  }

  it should "fail convert unsupported requests (strict)" in new BaseFixture {

    implicit val mat = SystemMaterializer(system)
    implicit val timeout = Timeout(30.seconds)

    val request = HttpRequest(
      HttpMethods.POST,
      Uri("http://xyz/client/2.0/template/folder"),
      Nil,
      HttpEntity("false")
    )

    val msg: HttpRequestStrictV1 = new HttpRequestStrictV1(Seq("client", "2.0", "template", "folder"), request)
    httpService ! msg

    expectMsgPF(30.seconds) {
      case HttpResultStrictV1(attributes, content, contentType) =>
        contentType.mediaType.toString shouldBe "text/plain"
        content.utf8String shouldBe ServiceUnavailable.getMessage
        attributes.statusCode shouldBe StatusCodes.SERVICE_UNAVAILABLE.intValue
    }

  }

  it should "send all paths to register" in new BaseFixture {

    httpService ! AllHttpRegisterPaths

    expectMsgPF(30.seconds) {
      case HttpRegisterPaths(paths) =>
        paths should have size 1
        paths.head.path shouldBe List("client", "2.0", "template")
    }

  }

  trait BaseFixture {

    val executor = TestProbe("Executor")

    val httpService = system.actorOf(HttpService.props(executor.ref))
  }

}
