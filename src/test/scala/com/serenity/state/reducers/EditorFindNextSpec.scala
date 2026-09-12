package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.CursorViewport
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for opening find/goto-line from editor events and advancing `FindNext` through stored results --
  * including seeding from existing find state, grapheme-safe match skipping, wrapped-line scrolling to a match, and
  * invalidating stale results after an edit (extracted from the former monolithic `EditorEventReducerSpec`, #1442).
  * `EditorFindEventReducerSpec` covers `OpenReplace` and `FindNext`'s no-op paths, deliberately not duplicated here.
  */
class EditorFindNextSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorEventReducer" should "open the goto-line modal from editor events even when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("alpha\nbeta")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(1, 2)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(OpenGotoLine, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open find from editor events even when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("alpha\nbeta")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(1, 2)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(OpenFind, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.Find("", Nil, 0)))
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(CursorPosition(0, 1)), SurfacePlacement.BelowCursor)
    )
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "seed find from the active buffer's existing find state" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("alpha\nbeta\nalpha")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(2, 0))),
              findState = Some(FindState("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(OpenFind, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(Modal.Find("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1))
    )
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(CursorPosition(2, 0)), SurfacePlacement.BelowCursor)
    )
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "advance find-next from the stored query even when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("match alpha\nbeta\nmatch gamma")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(2, 2))),
              findState = Some(FindState("match", List(FindResult(0, 0), FindResult(2, 0)), 0)),
              viewport = AppState.initial.persisted.buffers(bufferId).viewport.copy(visibleLines = 2)
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(FindNext, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(2, 0))
    buffer.findState shouldBe Some(FindState("match", List(FindResult(0, 0), FindResult(2, 0)), 1))
  }

  it should "advance find-next through multiple occurrences on one line by column" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("needle then needle")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
              findState = Some(FindState("needle", List(FindResult(0, 0), FindResult(0, "needle then ".length)), 0))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(FindNext, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(0, "needle then ".length))
    buffer.findState shouldBe Some(
      FindState("needle", List(FindResult(0, 0), FindResult(0, "needle then ".length)), 1)
    )
  }

  it should "drop find-next matches that split a grapheme cluster" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("cafe\u0301!")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
              findState = Some(FindState("\u0301", List(FindResult(0, 4)), 0))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(FindNext, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.findState shouldBe None
    buffer.editing.cursors shouldBe List(CursorPosition(0, 0))
  }

  it should "drop find-next matches that split a regional-indicator flag pair" in {
    val paneId         = PaneId(0)
    val bufferId       = BufferId(0)
    val flag           = "\uD83C\uDDFA\uD83C\uDDF8"
    val firstIndicator = flag.substring(0, 2)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope(s"a$flag!")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
              findState = Some(FindState(firstIndicator, List(FindResult(0, 1)), 0))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(FindNext, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.findState shouldBe None
    buffer.editing.cursors shouldBe List(CursorPosition(0, 0))
  }

  it should "scroll wrapped text to the selected find-next visual row" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val prefix       = List.fill(80)("wrapped").mkString(" ")
    val content      = s"first $prefix needle"
    val needleColumn = content.indexOf("needle")
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope(content)),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
              findState = Some(FindState("needle", List(FindResult(0, needleColumn)), 0)),
              viewport = Viewport(0, 0, visibleLines = 3, visibleColumns = 12)
            )
        )
      )
    )

    val reducedState = EditorEventReducer.reduce(FindNext, paneId, initialState).state
    val updatedState = CursorViewport.ensureVisibleCursors(initialState, reducedState)
    val buffer       = updatedState.persisted.buffers(bufferId)
    val cursor       = buffer.editing.cursors.head
    val font         = FontLoader.previewTextFont(updatedState.persisted.config.editorConfig.fontConfig)
    val wrapPx =
      TextLayoutSnapshot.gridWrapWidthPx(
        buffer.viewport.visibleColumns,
        updatedState.persisted.config.editorConfig.fontConfig
      )
    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, wrapPx, font)

    cursor shouldBe CursorPosition(0, needleColumn)
    buffer.viewport.topLine shouldBe 0
    buffer.viewport.topVisualLine should be > 0
    withClue(
      s"viewport=${buffer.viewport} cursor=$cursor visualLines=${snapshot.visualLines.map(line => (line.startColumn, line.endColumn))}"
    ) {
      snapshot.visualLines.exists(line =>
        line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn
      ) shouldBe true
    }
  }

  it should "invalidate stored find results after text is inserted" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("needle\nneedle")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
              findState = Some(FindState("needle", List(FindResult(0, 0), FindResult(1, 0)), 0))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(InsertChar('x'), paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "xneedle\nneedle"
    buffer.findState shouldBe None
  }

  it should "clear stored find-all results when an edit removes the last match" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("needle")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, "needle".length))),
              findState = Some(FindState("needle", List(FindResult(0, 0)), 0))
            )
        )
      )
    )

    val afterFirstDelete = EditorEventReducer.reduce(DeleteBackward, paneId, initialState).state
    val buffer           = afterFirstDelete.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "needl"
    buffer.findState shouldBe None
  }
