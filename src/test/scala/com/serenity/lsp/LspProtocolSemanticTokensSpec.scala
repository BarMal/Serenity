package com.serenity.lsp

import com.serenity.lsp.client.{DocumentUri, LspProtocol}
import com.serenity.lsp.model.{SemanticTokensEdit, SemanticTokensFeatures, SemanticTokensResult}
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The wire side of `full/delta` and `range` semantic tokens (LSP 3.17 §3.17.7): what a server offers, how requests are
  * built, how results decode into primitive arrays, and how a delta applies to the result it is relative to.
  */
class LspProtocolSemanticTokensSpec extends AnyFlatSpec with Matchers:

  private val documentUri = DocumentUri("file:///foo/Bar.scala")

  private def providerWith(fields: (String, Json)*): Json =
    Json.obj(
      "capabilities" -> Json.obj("semanticTokensProvider" -> Json.obj(fields*))
    )

  private def ints(values: Int*): IArray[Int] = IArray.from(values)

  "LspProtocol.parseSemanticTokensFeatures" should "read full.delta and range from an options object" in {
    val result = providerWith("full" -> Json.obj("delta" -> true.asJson), "range" -> true.asJson)

    LspProtocol.parseSemanticTokensFeatures(result) shouldBe
      SemanticTokensFeatures(full = true, delta = true, range = true)
  }

  it should "read a boolean full as a full request without delta" in {
    LspProtocol.parseSemanticTokensFeatures(providerWith("full" -> true.asJson)) shouldBe
      SemanticTokensFeatures(full = true, delta = false, range = false)
  }

  it should "read an empty options object as the request being offered" in {
    LspProtocol.parseSemanticTokensFeatures(providerWith("full" -> Json.obj(), "range" -> Json.obj())) shouldBe
      SemanticTokensFeatures(full = true, delta = false, range = true)
  }

  it should "read a server offering range alone" in {
    LspProtocol.parseSemanticTokensFeatures(providerWith("range" -> true.asJson)) shouldBe
      SemanticTokensFeatures(full = false, delta = false, range = true)
  }

  it should "read full as not offered when it is false, whatever delta says" in {
    LspProtocol.parseSemanticTokensFeatures(providerWith("full" -> false.asJson, "range" -> true.asJson)) shouldBe
      SemanticTokensFeatures(full = false, delta = false, range = true)
  }

  it should "assume a full request from a provider that names none" in {
    LspProtocol.parseSemanticTokensFeatures(providerWith()) shouldBe SemanticTokensFeatures.FullOnly
  }

  it should "assume a full request when the capability is absent" in {
    LspProtocol.parseSemanticTokensFeatures(Json.obj("capabilities" -> Json.obj())) shouldBe
      SemanticTokensFeatures.FullOnly
  }

  "LspProtocol request params" should "name the previous result id in a delta request" in {
    val params = LspProtocol.semanticTokensDeltaParams(documentUri, "42")

    params.hcursor.downField("textDocument").downField("uri").as[String].toOption shouldBe Some(documentUri.value)
    params.hcursor.downField("previousResultId").as[String].toOption shouldBe Some("42")
  }

  it should "cover whole lines in a range request, up to the start of the line after the last" in {
    val range = LspProtocol.semanticTokensRangeParams(documentUri, 10, 40).hcursor.downField("range")

    range.downField("start").as[(Int, Int)](using positionDecoder).toOption shouldBe Some((10, 0))
    range.downField("end").as[(Int, Int)](using positionDecoder).toOption shouldBe Some((41, 0))
  }

  private val positionDecoder: io.circe.Decoder[(Int, Int)] =
    io.circe.Decoder.instance(cursor =>
      for
        line      <- cursor.downField("line").as[Int]
        character <- cursor.downField("character").as[Int]
      yield (line, character)
    )

  "LspProtocol.parseSemanticTokensResult" should "read the data and result id of a full result into a primitive array" in {
    val result = Json.obj("resultId" -> "7".asJson, "data" -> List(0, 0, 3, 0, 0).asJson)

    LspProtocol.parseSemanticTokensResult(result) match
      case Some(SemanticTokensResult.Full(resultId, data)) =>
        resultId shouldBe Some("7")
        data.toList shouldBe List(0, 0, 3, 0, 0)
      case other => fail(s"expected a full result, got $other")
  }

  it should "read a full result without a result id" in {
    LspProtocol.parseSemanticTokensResult(Json.obj("data" -> Json.arr())).map(_.resultId) shouldBe Some(None)
  }

  it should "read the edits of a delta result" in {
    val result = Json.obj(
      "resultId" -> "8".asJson,
      "edits" -> Json.arr(
        Json.obj("start" -> 5.asJson, "deleteCount"  -> 5.asJson, "data" -> List(1, 0, 6, 1, 0).asJson),
        Json.obj("start" -> 15.asJson, "deleteCount" -> 0.asJson)
      )
    )

    LspProtocol.parseSemanticTokensResult(result) match
      case Some(SemanticTokensResult.Delta(resultId, edits)) =>
        resultId shouldBe Some("8")
        edits.map(edit => (edit.start, edit.deleteCount, edit.data.toList)) shouldBe
          List((5, 5, List(1, 0, 6, 1, 0)), (15, 0, Nil))
      case other => fail(s"expected a delta result, got $other")
  }

  it should "reject a result whose data holds something other than a non-negative integer" in {
    LspProtocol.parseSemanticTokensResult(Json.obj("data" -> Json.arr(0.asJson, (-1).asJson))) shouldBe None
    LspProtocol.parseSemanticTokensResult(Json.obj("data" -> Json.arr(0.asJson, "x".asJson))) shouldBe None
    LspProtocol.parseSemanticTokensResult(Json.obj("data" -> Json.arr(0.asJson, 1.5.asJson))) shouldBe None
  }

  it should "reject a result with neither data nor edits, such as null" in {
    LspProtocol.parseSemanticTokensResult(Json.Null) shouldBe None
    LspProtocol.parseSemanticTokensResult(Json.obj("resultId" -> "1".asJson)) shouldBe None
  }

  "SemanticTokensResult.applyEdits" should "replace, insert and delete against offsets of the previous data" in {
    val previous = ints(0, 0, 3, 0, 0, 1, 0, 4, 1, 0, 1, 0, 5, 2, 0)
    val edits = List(
      SemanticTokensEdit(5, 5, ints(1, 0, 6, 1, 0)),
      SemanticTokensEdit(15, 0, ints(1, 2, 3, 0, 0))
    )

    SemanticTokensResult.applyEdits(previous, edits).map(_.toList) shouldBe
      Some(List(0, 0, 3, 0, 0, 1, 0, 6, 1, 0, 1, 0, 5, 2, 0, 1, 2, 3, 0, 0))
  }

  it should "apply edits given out of order as if they were in order of their start" in {
    val previous = ints(0, 0, 3, 0, 0, 1, 0, 4, 1, 0)
    val edits    = List(SemanticTokensEdit(5, 5, ints()), SemanticTokensEdit(2, 1, ints(9)))

    SemanticTokensResult.applyEdits(previous, edits).map(_.toList) shouldBe Some(List(0, 0, 9, 0, 0))
  }

  it should "return the previous data for no edits" in {
    SemanticTokensResult.applyEdits(ints(1, 2, 3, 4, 5), Nil).map(_.toList) shouldBe Some(List(1, 2, 3, 4, 5))
  }

  it should "reject overlapping edits" in {
    val edits = List(SemanticTokensEdit(0, 5, ints()), SemanticTokensEdit(3, 1, ints()))

    SemanticTokensResult.applyEdits(ints(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), edits) shouldBe None
  }

  it should "reject an edit that reaches past the previous data" in {
    SemanticTokensResult.applyEdits(ints(1, 2, 3), List(SemanticTokensEdit(2, 5, ints()))) shouldBe None
    SemanticTokensResult.applyEdits(ints(1, 2, 3), List(SemanticTokensEdit(9, 0, ints(1)))) shouldBe None
  }
