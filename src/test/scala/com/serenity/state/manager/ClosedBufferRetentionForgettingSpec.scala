package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.{Diagnostic, LspPosition, LspRange, SemanticToken, SemanticTokenData}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, UndoState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClosedBufferRetentionForgettingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val initial = AppState.initial
  private val first   = initial.persisted.buffers.values.head
  private val closed =
    first.copy(id = BufferId(41), document = first.document.copy(filePath = Some(Path.of("/tmp/closed.txt"))))

  private def withBuffers(state: AppState, buffers: Map[BufferId, Buffer]): AppState =
    state.copy(persisted = state.persisted.copy(buffers = buffers))

  private val range      = LspRange(LspPosition(0, 0), LspPosition(0, 1))
  private val diagnostic = Diagnostic(range, None, "message")
  private val token      = SemanticToken(0, 0, 1, "variable", Set.empty)
  private val tokens     = SemanticTokenData.from(List(token))

  private val withClosedBuffer = withBuffers(initial, initial.persisted.buffers + (closed.id -> closed))
  private val closedUri        = withClosedBuffer.runtime.bufferIndexMemos.uriFor(closed)
  private val otherUri         = DocumentUri("file:///other.txt")

  private val languageData =
    withClosedBuffer.copy(runtime =
      withClosedBuffer.runtime.copy(languageService =
        LanguageServiceState(
          diagnosticsState = DiagnosticsState(
            diagnostics = Map(closedUri -> List(diagnostic), otherUri -> List(diagnostic))
          ),
          semanticTokensState = SemanticTokensState(
            byUri = Map(closedUri -> tokens, otherUri -> tokens),
            unavailableUris = Set(closedUri, otherUri)
          )
        )
      )
    )

  private val undo =
    UndoState(undoStack = Vector(HistoryEntry.BufferEdit(closed.id, PaneId(0), BufferSnapshot.fromBuffer(closed))))

  "forgetting" should "hand back the very model it was given when no buffer was removed" in {
    val next = Model(initial.copy(runtime = initial.runtime.copy(nextBufferId = BufferId(99))), undo)

    ClosedBufferRetention.forgetting(initial, next) should be theSameInstanceAs next
  }

  it should "hand back the very model it was given when buffers were only added" in {
    val next = Model(withClosedBuffer, undo)

    ClosedBufferRetention.forgetting(initial, next) should be theSameInstanceAs next
  }

  it should "drop the undo entries, diagnostics, semantic tokens and spell-check data of a removed buffer's document" in {
    val before = languageData
    val after  = withBuffers(languageData, initial.persisted.buffers)

    val forgotten = ClosedBufferRetention.forgetting(before, Model(after, undo))

    forgotten.undo.undoStack shouldBe empty
    val languageService = forgotten.app.runtime.languageService
    languageService.diagnosticsState.diagnostics.keySet shouldBe Set(otherUri)
    languageService.semanticTokensState.byUri.keySet shouldBe Set(otherUri)
    languageService.semanticTokensState.unavailableUris shouldBe Set(otherUri)
  }

  it should "keep a document's language data while another open buffer still shows it" in {
    val twin   = closed.copy(id = BufferId(42))
    val before = withBuffers(languageData, languageData.persisted.buffers + (twin.id -> twin))
    val after  = withBuffers(before, before.persisted.buffers - closed.id)

    val forgotten = ClosedBufferRetention.forgetting(before, Model(after, undo))

    forgotten.app.runtime.languageService.diagnosticsState.diagnostics.keySet shouldBe Set(closedUri, otherUri)
    forgotten.undo.undoStack shouldBe empty
  }
