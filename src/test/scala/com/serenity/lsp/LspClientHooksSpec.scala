package com.serenity.lsp

import com.serenity.lsp.client.{
  DocumentUri,
  LspApplyEditRequest,
  LspApplyEditResult,
  LspClientHooks,
  LspMessageLevel,
  LspProgressUpdate,
  LspServerMessage
}
import com.serenity.lsp.model.{LspPosition, LspProgress, LspRange, LspTextEdit}
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LspClientHooksSpec extends AnyFlatSpec with Matchers:

  "LspClientHooks.parseMessage" should "map the message type onto a level, defaulting to log" in {
    LspClientHooks.parseMessage(Json.obj("type" -> 1.asJson, "message" -> "boom".asJson)) shouldBe
      Some(LspServerMessage(LspMessageLevel.Error, "boom"))
    LspClientHooks.parseMessage(Json.obj("type" -> 3.asJson, "message" -> "hi".asJson)) shouldBe
      Some(LspServerMessage(LspMessageLevel.Info, "hi"))
    LspClientHooks.parseMessage(Json.obj("message" -> "bare".asJson)) shouldBe
      Some(LspServerMessage(LspMessageLevel.Log, "bare"))
    LspClientHooks.parseMessage(Json.obj("type" -> 1.asJson)) shouldBe None
  }

  "LspClientHooks.parseProgress" should "read begin, report and end with string or numeric tokens" in {
    def params(token: Json, value: Json) = Json.obj("token" -> token, "value" -> value)

    LspClientHooks.parseProgress(
      params("t".asJson, Json.obj("kind" -> "begin".asJson, "title" -> "Indexing".asJson, "percentage" -> 0.asJson))
    ) shouldBe Some(LspProgressUpdate("t", LspProgress.Begin("Indexing", None, Some(0))))
    LspClientHooks.parseProgress(
      params(7.asJson, Json.obj("kind" -> "report".asJson, "message" -> "half".asJson, "percentage" -> 50.asJson))
    ) shouldBe Some(LspProgressUpdate("7", LspProgress.Report(Some("half"), Some(50))))
    LspClientHooks.parseProgress(params("t".asJson, Json.obj("kind" -> "end".asJson))) shouldBe
      Some(LspProgressUpdate("t", LspProgress.End(None)))
  }

  it should "skip progress that is not work-done progress" in {
    LspClientHooks.parseProgress(
      Json.obj("token" -> "t".asJson, "value" -> Json.obj("items" -> Json.arr()))
    ) shouldBe None
    LspClientHooks.parseProgress(
      Json.obj("token" -> "t".asJson, "value" -> Json.obj("kind" -> "begin".asJson))
    ) shouldBe None
  }

  "LspClientHooks.parseApplyEdit" should "read the label and the edit in either WorkspaceEdit form" in {
    val edit = Json.obj(
      "newText" -> "x".asJson,
      "range" -> Json.obj(
        "start" -> Json.obj("line" -> 0.asJson, "character" -> 1.asJson),
        "end"   -> Json.obj("line" -> 0.asJson, "character" -> 2.asJson)
      )
    )
    val expected = Map(
      DocumentUri("file:///a") -> List(LspTextEdit(LspRange(LspPosition(0, 1), LspPosition(0, 2)), "x"))
    )
    val changes =
      Json.obj("label" -> "Fix".asJson, "edit" -> Json.obj("changes" -> Json.obj("file:///a" -> Json.arr(edit))))

    LspClientHooks.parseApplyEdit(changes) shouldBe Some(LspApplyEditRequest(Some("Fix"), expected))
    LspClientHooks.parseApplyEdit(Json.obj()) shouldBe None
  }

  "LspClientHooks.applyEditResponse" should "include the failure reason only when there is one" in {
    LspClientHooks.applyEditResponse(LspApplyEditResult(applied = true, None)) shouldBe
      Json.obj("applied" -> true.asJson)
    LspClientHooks.applyEditResponse(LspApplyEditResult(applied = false, Some("nope"))) shouldBe
      Json.obj("applied" -> false.asJson, "failureReason" -> "nope".asJson)
  }
