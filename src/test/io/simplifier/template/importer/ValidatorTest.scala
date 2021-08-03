package io.simplifier.template.importer

import akka.actor.testkit.typed.scaladsl.{ManualTime, ScalaTestWithActorTestKit, TestProbe}
import io.simplifier.template.importer.Importer.{Import, Validated}
import io.simplifier.template.importer.Validator.{MissingFeatures, Ok, Skipped, Validate}
import org.apache.commons.io.IOUtils
import org.scalatest.flatspec.AnyFlatSpecLike

import java.nio.charset.StandardCharsets
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class ValidatorTest extends ScalaTestWithActorTestKit(ManualTime.config) with AnyFlatSpecLike {

  val manualTime: ManualTime = ManualTime()

  "A Validator" should "determine which features are needed for import" in {
    val importer = TestProbe[Import]("importer")
    val validator = testKit.spawn(Validator(access))
    validator ! Validate("more", importer.ref)
    val validated = importer.expectMessageType[Validated]
    validated.validationResult.features.count(_.isInstanceOf[Ok]) shouldBe 5
    validated.validationResult.features.count(_.isInstanceOf[Skipped]) shouldBe 51
  }

  it should "mark features failed if dependent features are missing" in {
    val importer = TestProbe[Import]("importer")
    val validator = testKit.spawn(Validator(access))
    validator ! Validate("missing", importer.ref)
    val validated = importer.expectMessageType[Validated]
    validated.validationResult.features.count(_.isInstanceOf[Ok]) shouldBe 0
    validated.validationResult.features.count(_.isInstanceOf[MissingFeatures]) shouldBe 1
    val MissingFeatures(artifact, missing) = validated.validationResult.features.filter(_.isInstanceOf[MissingFeatures]).head
    artifact.feature shouldBe "com.itizzimo.appServer.transport.feature.TemplateFeature"
    artifact.id shouldBe "CECD0D28D1D9E1A7CEFF6BA2024142454E8907E5A7F38B74594D5BDBF438FD70"
    missing.size shouldBe 2
    missing.map(_.id).toSet shouldBe Set("BA1C4E58EE5AFAEBA46E3F63E2D6E40DF0F0F13230F055E02F4BFE73EC63EDB9",
                                         "1C4AE054DEC09A21536D0EF7828A00A0162E3937AE26A1631644904107BD6F0A")
    missing.map(_.feature).toSet shouldBe Set("com.itizzimo.appServer.transport.feature.DataTypeFeature")
  }

  it should "mark features failed if dependent feature artifact are malformed" in {
    val importer = TestProbe[Import]("importer")
    val validator = testKit.spawn(Validator(access))
    validator ! Validate("error", importer.ref)
    val validated = importer.expectMessageType[Validated]
    validated.validationResult.features.count(_.isInstanceOf[Ok]) shouldBe 0
    validated.validationResult.features.count(_.isInstanceOf[Validator.Error]) shouldBe 1
    val Validator.Error(artifact, missing) = validated.validationResult.features.filter(_.isInstanceOf[Validator.Error]).head
    artifact.feature shouldBe "com.itizzimo.appServer.transport.feature.TemplateFeature"
    artifact.id shouldBe "CECD0D28D1D9E1A7CEFF6BA2024142454E8907E5A7F38B74594D5BDBF438FD70"
    missing.getMessage shouldBe "Feature malformed: Structure parameter [fields] malformed of data type 35595BC0D93EFF3EE2453477349FE81964D647AC569D64BB37E8DECFA40188BB."
  }

  val access = new PersistenceAccess {
    val transports = Map(
      "more/artifacts.json" -> IOUtils.resourceToString("/artifactsAndMore.json", StandardCharsets.UTF_8),
      "missing/artifacts.json" -> IOUtils.resourceToString("/artifactsMissing.json", StandardCharsets.UTF_8),
      "error/artifacts.json" -> IOUtils.resourceToString("/artifactsError.json", StandardCharsets.UTF_8)
    )
    override def readBytes(name: String): Try[Array[Byte]] = ???

    override def writeBytes(name: String, data: Array[Byte]): Try[PersistenceAccess.PersistenceResponse] = ???

    override def readStringFuture(name: String)(implicit ec: ExecutionContext): Future[String] =
      Future.successful(transports(name))

    override def readHash(name: String): Try[String] = ???
  }

}
