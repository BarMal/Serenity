package com.serenity.session

import _root_.io.circe.parser
import _root_.io.circe.syntax.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A buffer's chosen indentation survives a session; what its text shows is worked out again on restore (#1996). */
class SessionIndentationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def withContent(text: String, indentation: DocumentIndentation): Buffer =
    val fresh = Buffer.fromString(BufferId(1), text)
    fresh.copy(document = fresh.document.copy(indentation = indentation, isDirty = true))

  private def restored(buffer: Buffer): Buffer =
    val encoded = SessionBuffer.fromBuffer(buffer).asJson.noSpaces
    val decoded = parser.decode[SessionBuffer](encoded).getOrElse(fail("a session buffer should decode"))
    SessionBuffer.toBuffer(decoded)

  "A session" should "restore the indentation the user chose" in {
    val chosen = DocumentIndentation().withChosen(Some(IndentationSettings(tabSize = 8, insertSpaces = false)))

    restored(withContent("a\n", chosen)).document.indentation.chosen shouldBe chosen.chosen
  }

  it should "detect the indentation of the restored text again" in {
    val buffer = withContent("a\n  b\n    c\n", DocumentIndentation())

    restored(buffer).document.indentation.detected shouldBe
      IndentationHint(insertSpaces = Some(true), tabSize = Some(2))
  }

  it should "restore a buffer with no choice without one" in {
    restored(withContent("a\n", DocumentIndentation())).document.indentation.chosen shouldBe IndentationHint.Empty
  }

  it should "restore a session written before indentation was recorded" in {
    val legacy = """{"id":1,"filePath":null,"isDirty":false,"language":null,"isNewEmpty":false,
                   |"cursors":[{"line":0,"column":0}],
                   |"viewport":{"leftColumn":0,"topLine":0,"visibleColumns":80,"visibleLines":24}}""".stripMargin

    parser.decode[SessionBuffer](legacy).map(_.indentation) shouldBe Right(None)
  }

  it should "ignore a stored choice it cannot read" in {
    val session = SessionBuffer.fromBuffer(withContent("a\n", DocumentIndentation())).copy(indentation = Some("wide:4"))

    SessionBuffer.toBuffer(session).document.indentation.chosen shouldBe IndentationHint.Empty
  }
