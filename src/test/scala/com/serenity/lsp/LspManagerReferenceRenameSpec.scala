package com.serenity.lsp

import cats.effect.IO
import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{LspPosition, LspRange, LspTextEdit}
import com.serenity.state.models.CursorPosition
import com.serenity.testkit.VirtualTime.runVirtual
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `textDocument/references` and `textDocument/rename` request/response handling, split out of [[LspManagerSpec]]
  * (which covers every other request kind) to keep both files under the architecture ratchet's file-length target.
  */
class LspManagerReferenceRenameSpec extends AnyFlatSpec with Matchers with LspManagerSpecFixture:

  "LspManager" should "send a references request and emit the locations it receives" in {
    val anchor = CursorPosition(0, 1)
    runVirtual(
      harness()
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(
              Some(LspEffect.ReferencesRequested(uri, LanguageId.Scala, 0, 1, anchor, "Foo"))
            )
            request <- takeMessage(manager.connection)
            _ = request.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/references")
            _ <- manager.connection.handleIncomingJson(
              response(
                requestId(request),
                Json.arr(
                  Json.obj(
                    "uri" -> uri.asJson,
                    "range" -> Json.obj(
                      "start" -> Json.obj("line" -> 2.asJson, "character" -> 4.asJson),
                      "end"   -> Json.obj("line" -> 2.asJson, "character" -> 7.asJson)
                    )
                  )
                )
              )
            )
            _      <- manager.eventApplied.get
            _      <- manager.stop
            events <- manager.events.get
          yield events shouldBe List(
            LspEvent.LspReferencesReceived("Foo", List(uri -> LspPosition(2, 4)), anchor)
          )
        }
    )
  }

  it should "discard a references response after its document version changes" in {
    val anchor = CursorPosition(0, 1)
    runVirtual(
      harness()
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(
              Some(LspEffect.ReferencesRequested(uri, LanguageId.Scala, 0, 1, anchor, "Foo"))
            )
            request <- takeMessage(manager.connection)
            _ <- manager.effects.offer(Some(LspEffect.FileChanged(uri, LanguageId.Scala, "object Foo2", version = 2)))
            _ <- takeMessage(manager.connection)
            _ <- manager.connection.handleIncomingJson(response(requestId(request), Json.arr()))
            events <- manager.events.get
            _ = events shouldBe Nil
            _ <- manager.stop
          yield succeed
        }
    )
  }

  it should "send a rename request and emit the edits it receives" in {
    val anchor = CursorPosition(0, 1)
    runVirtual(
      harness()
        .use { manager =>
          for
            _ <- open(manager)
            _ <- manager.effects.offer(
              Some(LspEffect.RenameRequested(uri, LanguageId.Scala, 0, 1, anchor, "renamed"))
            )
            request <- takeMessage(manager.connection)
            _ = request.hcursor.downField("method").as[String].toOption shouldBe Some("textDocument/rename")
            _ = request.hcursor.downField("params").downField("newName").as[String].toOption shouldBe Some("renamed")
            _ <- manager.connection.handleIncomingJson(
              response(
                requestId(request),
                Json.obj(
                  "changes" -> Json.obj(
                    uri -> Json.arr(
                      Json.obj(
                        "range" -> Json.obj(
                          "start" -> Json.obj("line" -> 0.asJson, "character" -> 7.asJson),
                          "end"   -> Json.obj("line" -> 0.asJson, "character" -> 10.asJson)
                        ),
                        "newText" -> "renamed".asJson
                      )
                    )
                  )
                )
              )
            )
            _      <- manager.eventApplied.get
            _      <- manager.stop
            events <- manager.events.get
          yield events shouldBe List(
            LspEvent.LspRenameReceived(
              Map(uri -> List(LspTextEdit(LspRange(LspPosition(0, 7), LspPosition(0, 10)), "renamed"))),
              anchor
            )
          )
        }
    )
  }

  it should "emit no event for a references request when no LSP server is available" in {
    val anchor = CursorPosition(0, 1)
    runVirtual(
      harness(serverAvailable = false)
        .use { manager =>
          for
            _ <- manager.effects.offer(
              Some(LspEffect.ReferencesRequested(uri, LanguageId.Scala, 0, 1, anchor, "Foo"))
            )
            _      <- manager.stop
            events <- manager.events.get
          yield events shouldBe Nil
        }
    )
  }

  it should "emit no event for a rename request when no LSP server is available" in {
    val anchor = CursorPosition(0, 1)
    runVirtual(
      harness(serverAvailable = false)
        .use { manager =>
          for
            _ <- manager.effects.offer(
              Some(LspEffect.RenameRequested(uri, LanguageId.Scala, 0, 1, anchor, "renamed"))
            )
            _      <- manager.stop
            events <- manager.events.get
          yield events shouldBe Nil
        }
    )
  }
