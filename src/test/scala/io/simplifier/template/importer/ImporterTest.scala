package io.simplifier.template.importer

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.scaladsl.Behaviors
import akka.testkit.TestProbe
import io.simplifier.template.design.transport.Feature.TransportFeature
import io.simplifier.template.importer.Importer.Validated
import io.simplifier.template.importer.Validator.{Error, MissingFeature, MissingFeatures, Ok, Skipped, Validate, ValidationMessage, ValidationResult, ValidatorFactory}
import org.scalatest.flatspec.AnyFlatSpecLike
import io.circe.Json._
import io.circe.syntax._
import io.simplifier.transport.messages.v1.TransportCommandV1.ValidateImportV1
import akka.actor.typed.scaladsl.adapter._
import io.simplifier.transport.messages.v1.ImportStatusV1.{FeatureStatusV1, ImportValidationCompleteV1}

class ImporterTest extends ScalaTestWithActorTestKit with AnyFlatSpecLike {

  "An Importer" should "validate an import" in new BaseFixture {

    implicit val system = testKit.system.toClassic
    val receiver = TestProbe("test")
    val transportId = "transportId"
    val path = "path"
    val importer = testKit.spawn(Importer(sendCompleteAndSkipped))

    importer ! ValidateImportV1(transportId, path, receiver.ref)
    val response = receiver.receiveN(1)
    response.head match {
      case ImportValidationCompleteV1(id, features, _) =>
        id shouldBe transportId
        features should have size 4
        features.get(ok.artifact.feature) shouldBe Some(FeatureStatusV1(1, 0, 0, 1, Seq.empty[String]))
        features.get(skipped1.artifact.feature) shouldBe Some(FeatureStatusV1(0, 0, 2, 2, Seq.empty[String]))
        features.get(error.artifact.feature) shouldBe Some(FeatureStatusV1(0, 1, 0, 1, Seq("uh-oh!")))
        features.get(missing.artifact.feature) shouldBe Some(FeatureStatusV1(0, 1, 0, 1,
                   Seq("Missing Feature missing1Id of type missing1",
                       "Missing Feature missing2Id of type missing2",
                       "Missing Feature missing3Id of type missing3")))
      case msg =>
        msg shouldBe a [ImportValidationCompleteV1]
    }


  }

  trait BaseFixture {

    val ok = Ok(TransportFeature("okId", "okFeature", "Ok", 1, "Ok".asJson), Map())
    val skipped1 = Skipped(TransportFeature("skipped1Id", "skippedFeature", "Skipped1", 1, "Skipped1".asJson))
    val skipped2 = Skipped(TransportFeature("skipped2Id", "skippedFeature", "Skipped2", 1, "Skipped2".asJson))
    val error = Error(TransportFeature("errorId", "errorFeature", "Error", 1, "Error".asJson), new RuntimeException("uh-oh!"))
    val missing = MissingFeatures(TransportFeature("missingId", "missingFeature", "Missing", 1, "Missing".asJson), Set(
      MissingFeature("missing1", "missing1Id"),
      MissingFeature("missing2", "missing2Id"),
      MissingFeature("missing3", "missing3Id")
    ))
    def sendCompleteAndSkipped: ValidatorFactory = () =>

      Behaviors.receiveMessage[ValidationMessage] { msg => msg match {
        case Validate(_, replyTo) =>
          replyTo ! Validated(ValidationResult(Seq(ok, skipped1, skipped2, error, missing)))
          Behaviors.same
        }
      }

  }

}
