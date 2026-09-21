package com.serenity.lsp

import com.serenity.lsp.client.{DocumentUri, LspProtocol}
import com.serenity.lsp.model.{LspPosition, LspRange, LspTextEdit}
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `textDocument/references`/`textDocument/rename` params and response parsing, split out of [[LspProtocolSpec]] (which
  * covers every other LSP method) to keep both files under the architecture ratchet's file-length target.
  */
class LspProtocolReferenceRenameSpec extends AnyFlatSpec with Matchers:

  // ── References ────────────────────────────────────────────────────────────

  "LspProtocol" should "build references params carrying the position and includeDeclaration" in {
    val params = LspProtocol.referencesParams(DocumentUri("file:///foo/Bar.scala"), line = 3, character = 5)

    params.hcursor.downField("textDocument").downField("uri").as[String].toOption shouldBe
      Some("file:///foo/Bar.scala")
    params.hcursor.downField("position").downField("line").as[Int].toOption shouldBe Some(3)
    params.hcursor.downField("position").downField("character").as[Int].toOption shouldBe Some(5)
    params.hcursor.downField("context").downField("includeDeclaration").as[Boolean].toOption shouldBe Some(true)
  }

  it should "parse a list of reference locations, or None for a non-array result" in {
    val locations = Json.arr(
      Json.obj(
        "uri" -> "file:///foo/Bar.scala".asJson,
        "range" -> Json.obj(
          "start" -> Json.obj("line" -> 1.asJson, "character" -> 2.asJson),
          "end"   -> Json.obj("line" -> 1.asJson, "character" -> 5.asJson)
        )
      ),
      Json.obj(
        "uri" -> "file:///foo/Baz.scala".asJson,
        "range" -> Json.obj(
          "start" -> Json.obj("line" -> 9.asJson, "character" -> 0.asJson),
          "end"   -> Json.obj("line" -> 9.asJson, "character" -> 3.asJson)
        )
      )
    )

    val parsed = LspProtocol.parseReferencesLocations(locations)
    parsed.map(_.map(_.uri)) shouldBe Some(
      List(DocumentUri("file:///foo/Bar.scala"), DocumentUri("file:///foo/Baz.scala"))
    )
    parsed.map(_.map(_.range.start)) shouldBe Some(List(LspPosition(1, 2), LspPosition(9, 0)))

    LspProtocol.parseReferencesLocations(Json.arr()) shouldBe Some(Nil)
    LspProtocol.parseReferencesLocations(Json.Null) shouldBe None
  }

  // ── Rename ───────────────────────────────────────────────────────────────

  it should "build rename params carrying the position and new name" in {
    val params =
      LspProtocol.renameParams(DocumentUri("file:///foo/Bar.scala"), line = 2, character = 6, newName = "renamed")

    params.hcursor.downField("textDocument").downField("uri").as[String].toOption shouldBe
      Some("file:///foo/Bar.scala")
    params.hcursor.downField("position").downField("line").as[Int].toOption shouldBe Some(2)
    params.hcursor.downField("position").downField("character").as[Int].toOption shouldBe Some(6)
    params.hcursor.downField("newName").as[String].toOption shouldBe Some("renamed")
  }

  it should "parse a WorkspaceEdit's 'changes' form into edits keyed by document uri" in {
    val result = Json.obj(
      "changes" -> Json.obj(
        "file:///foo/Bar.scala" -> Json.arr(
          Json.obj(
            "range" -> Json.obj(
              "start" -> Json.obj("line" -> 0.asJson, "character" -> 0.asJson),
              "end"   -> Json.obj("line" -> 0.asJson, "character" -> 3.asJson)
            ),
            "newText" -> "renamed".asJson
          )
        )
      )
    )

    LspProtocol.parseWorkspaceEdit(result) shouldBe Map(
      DocumentUri("file:///foo/Bar.scala") -> List(
        LspTextEdit(LspRange(LspPosition(0, 0), LspPosition(0, 3)), "renamed")
      )
    )
  }

  it should "parse a WorkspaceEdit's 'documentChanges' form into edits keyed by document uri" in {
    val result = Json.obj(
      "documentChanges" -> Json.arr(
        Json.obj(
          "textDocument" -> Json.obj("uri" -> "file:///foo/Bar.scala".asJson, "version" -> 3.asJson),
          "edits" -> Json.arr(
            Json.obj(
              "range" -> Json.obj(
                "start" -> Json.obj("line" -> 1.asJson, "character" -> 2.asJson),
                "end"   -> Json.obj("line" -> 1.asJson, "character" -> 4.asJson)
              ),
              "newText" -> "x".asJson
            )
          )
        )
      )
    )

    LspProtocol.parseWorkspaceEdit(result) shouldBe Map(
      DocumentUri("file:///foo/Bar.scala") -> List(
        LspTextEdit(LspRange(LspPosition(1, 2), LspPosition(1, 4)), "x")
      )
    )
  }

  it should "parse an empty map from a WorkspaceEdit result with neither changes nor documentChanges" in {
    LspProtocol.parseWorkspaceEdit(Json.obj()) shouldBe Map.empty
    LspProtocol.parseWorkspaceEdit(Json.Null) shouldBe Map.empty
  }
