package com.serenity.state.models

import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.{ChangeSet, Rope}
import com.serenity.spellcheck.IncrementalSpellFixture.{*, given}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1924, #1929: a diagnostic goes with its text through every edit, before the server or the next spell check speaks.
  */
class DiagnosticsEditRemapSpec extends AnyFlatSpec with Matchers:

  private val text = "the cat sat\nteh dog"

  private def range(line: Int, from: Int, to: Int): LspRange = LspRange(LspPosition(line, from), LspPosition(line, to))

  private def editedState(state: AppState, from: Int, to: Int, insert: String): AppState =
    val buffer  = state.persisted.buffers(bufferId)
    val content = buffer.document.content
    val change  = ChangeSet.single(content.weight, from, to, insert)
    val updated = Rope(content.sliceString(0, from) + insert + content.sliceString(to, content.weight))
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, buffer.copy(document = buffer.document.edited(updated, change)))
      )
    )

  private def afterEdit(from: Int, to: Int, insert: String): List[Diagnostic] =
    val before = stateWith(Rope(text))
    published(DiagnosticsEditRemap.carried(before, editedState(before, from, to, insert)))

  "A spell-check squiggle" should "start on the misspelling flagged in the fixture" in {
    published(stateWith(Rope(text))).map(_.range) shouldBe List(range(1, 0, 3))
  }

  it should "move down a line when a line is inserted above it" in {
    afterEdit(0, 0, "hello\n").map(_.range) shouldBe List(range(2, 0, 3))
  }

  it should "move along the line when text is typed before it" in {
    afterEdit(12, 12, "a ").map(_.range) shouldBe List(range(1, 2, 5))
  }

  it should "move up a line when the line above it is deleted" in {
    afterEdit(0, 12, "").map(_.range) shouldBe List(range(0, 0, 3))
  }

  it should "clear at once when the flagged word is deleted" in {
    afterEdit(12, 15, "") shouldBe empty
  }

  it should "stay put when text is typed after it" in {
    afterEdit(19, 19, " cat").map(_.range) shouldBe List(range(1, 0, 3))
  }

  it should "keep covering its word when a character is typed inside it" in {
    afterEdit(14, 14, "x").map(_.range) shouldBe List(range(1, 0, 4))
  }

  "carried" should "leave a state whose buffers did not change as it was" in {
    val state = stateWith(Rope(text))
    DiagnosticsEditRemap.carried(state, state) should be theSameInstanceAs state
  }

  "An LSP diagnostic" should "move with an edit above it until the server republishes" in {
    val uri        = DocumentUri("buffer:1")
    val diagnostic = Diagnostic(range(1, 4, 7), Some(DiagnosticSeverity.Error), "bad", Some("lsp"), None)
    val before     = stateWith(Rope(text))
    val withLsp = before.copy(runtime =
      before.runtime.copy(languageService =
        before.runtime.languageService.copy(diagnosticsState =
          before.runtime.languageService.diagnosticsState.copy(diagnostics = Map(uri -> List(diagnostic)))
        )
      )
    )
    val moved = DiagnosticsEditRemap.carried(withLsp, editedState(withLsp, 0, 0, "\n\n"))
    moved.runtime.languageService.diagnosticsState.diagnostics(uri).map(_.range) shouldBe List(range(3, 4, 7))
  }

  "The annotation line index" should "report a diagnostic spanning lines 3 to 5 on each of them" in {
    val spanning = Diagnostic(
      LspRange(LspPosition(2, 4), LspPosition(4, 1)),
      Some(DiagnosticSeverity.Error),
      "spans",
      Some("lsp"),
      None
    )
    AnnotationLineIndex.diagnosticsBySpannedLine(List(spanning)).keySet shouldBe Set(2, 3, 4)
  }

  it should "not cover the line an exclusive end position sits at the start of" in {
    val ending = Diagnostic(
      LspRange(LspPosition(2, 4), LspPosition(4, 0)),
      Some(DiagnosticSeverity.Error),
      "ends at line start",
      Some("lsp"),
      None
    )
    AnnotationLineIndex.diagnosticsBySpannedLine(List(ending)).keySet shouldBe Set(2, 3)
  }
