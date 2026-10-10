package com.serenity.io

import scala.io.Source

import com.serenity.BuildInfo
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The CycloneDX SBOM attached to every release (#2017) describes the same runtime classpath the third-party notices
  * are rendered from, so the two documents cannot disagree about what ships.
  */
class SbomSpec extends AnyFlatSpec with Matchers:

  private def resourceText(path: String): String =
    Option(getClass.getResourceAsStream(path)).fold("") { stream =>
      val source = Source.fromInputStream(stream, "UTF-8")
      try source.mkString
      finally source.close()
    }

  private val sbom: Json =
    parse(resourceText("/sbom/Serenity.cdx.json")).fold(failure => fail(failure.getMessage), identity)

  private val components: List[Json] = sbom.hcursor.downField("components").as[List[Json]].getOrElse(Nil)

  private def text(json: Json, field: String): String =
    json.hcursor.get[String](field).getOrElse("")

  "The SBOM" should "be a CycloneDX document" in {
    text(sbom, "bomFormat") shouldBe "CycloneDX"
    text(sbom, "specVersion") should not be empty
  }

  it should "name the application at the version the build reports" in {
    val application = sbom.hcursor.downField("metadata").downField("component")

    application.get[String]("type") shouldBe Right("application")
    application.get[String]("version") shouldBe Right(BuildInfo.version)
  }

  it should "list exactly the runtime modules the third-party notices cover" in {
    val noticed = resourceText("/licences/runtime-modules.txt").linesIterator.filter(_.nonEmpty).toSet
    val listed  = components.map(component => s"${text(component, "group")}:${text(component, "name")}").toSet

    noticed should not be empty
    listed shouldBe noticed
  }

  it should "identify every component by version, package URL and SHA-256" in {
    components should not be empty
    components.foreach { component =>
      text(component, "version") should not be empty
      text(component, "purl") should startWith("pkg:maven/")
      val algorithms = component.hcursor.downField("hashes").as[List[Json]].getOrElse(Nil).map(text(_, "alg"))
      algorithms should contain("SHA-256")
    }
  }

  it should "carry no serial number or timestamp, so the same dependencies give the same bytes" in {
    sbom.hcursor.downField("serialNumber").succeeded shouldBe false
    sbom.hcursor.downField("metadata").downField("timestamp").succeeded shouldBe false
  }
