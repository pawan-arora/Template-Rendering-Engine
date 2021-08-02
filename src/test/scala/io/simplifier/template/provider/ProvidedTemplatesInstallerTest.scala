package io.simplifier.template.provider

import java.nio.charset.StandardCharsets

import io.circe.{HCursor, Json}
import org.apache.commons.io.IOUtils
import org.scalatest.flatspec.AnyFlatSpec
import io.circe.parser._
import org.scalatest.matchers.should.Matchers

class ProvidedTemplatesInstallerTest extends AnyFlatSpec with Matchers {

  "The hash name of a template" should "match the hash of the binref entry" in {

    val artifact = parse(IOUtils.resourceToString("/templateArtifact.json", StandardCharsets.UTF_8)).getOrElse(Json.Null)
    val template = IOUtils.resourceToByteArray("/Template-ComplexGreeting.html")

    val binRefHash = HCursor.fromJson(artifact)
      .downField("artifactData")
      .downField("template")
      .get[String]("Hash").getOrElse("")
    ProvidedTemplatesInstaller.hash(template) shouldBe binRefHash

  }

}
