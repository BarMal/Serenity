package com.serenity.state.reducers

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.CursorViewport
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for single-cursor viewport navigation: paging and scrolling, moving to the start/end of the file, and
  * wrapped-line vertical movement, including how the viewport itself is placed by `CursorViewport.ensureVisibleCursors`
  * rather than by the reducer (extracted from the former monolithic `EditorEventReducerSpec`, #1442).
  */
class EditorViewportNavigationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorEventReducer" should "move through wrapped code buffer visual rows using rendered wrap boundaries" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(true),
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = Rope("alpha beta gamma"), language = Some(LanguageId.JsonLang)),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
              viewport = Viewport(0, 0, visibleLines = 5, visibleColumns = 8)
            )
        )
      ),
      runtime = AppState.initial.runtime.copy(
        viewportSize = Some(ViewportSize(8, 6))
      )
    )

    val updatedState = com.serenity.VerticalNavSupport.dispatch(MoveDown, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(0, 6))
    buffer.viewport.leftColumn shouldBe 0
  }

  it should "restore each cursor's preferred column after moving through a shorter line" in {
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
                .copy(content = com.serenity.rope.Rope("abcdef\nxy\nabcdef")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(0, 4)))
            )
        )
      )
    )

    val afterFirstMove  = com.serenity.VerticalNavSupport.dispatch(MoveDown, paneId, initialState).state
    val afterSecondMove = com.serenity.VerticalNavSupport.dispatch(MoveDown, paneId, afterFirstMove).state
    val buffer          = afterSecondMove.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(2, 1), CursorPosition(2, 4))
  }

  it should "update viewport position for scroll events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)
    val bufferId     = initialState.persisted.layout.editorPanes(paneId).bufferId.get
    val seededState = initialState.copy(
      persisted = initialState.persisted.copy(
        buffers = initialState.persisted.buffers.updated(
          bufferId,
          initialState.persisted
            .buffers(bufferId)
            .copy(
              document = initialState.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("a\nb\nc\nd\ne\nf\ng")),
              viewport = initialState.persisted.buffers(bufferId).viewport.copy(visibleLines = 2)
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(ScrollDown(3), paneId, seededState).state

    updatedState.persisted.buffers(bufferId).viewport.topLine shouldBe 3
  }

  it should "move the cursor up by a visible page while clamping at the top" in {
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
                .copy(content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(4, 1))),
              viewport = AppState.initial.persisted.buffers(bufferId).viewport.copy(topLine = 3, visibleLines = 2)
            )
        )
      )
    )

    val reducedState = EditorEventReducer.reduce(PageUp, paneId, initialState).state
    val updatedState = CursorViewport.ensureVisibleCursors(initialState, reducedState)
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(2, 0))
    buffer.viewport.topLine shouldBe 1
  }

  it should "leave the viewport alone when a page move cannot move the cursor any further" in {
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
                .copy(content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(5, 0))),
              viewport = AppState.initial.persisted.buffers(bufferId).viewport.copy(topLine = 4, visibleLines = 2)
            )
        )
      )
    )

    // The cursor is already on the last line, so PageDown moves nothing -- and a viewport the reducer computed for
    // itself would scroll anyway. This is where that showed: the clamp `totalLines - visibleLines` is measured in
    // logical lines, goes negative for a wrapped document, and snapped the viewport back to the top of the file.
    val reducedState = EditorEventReducer.reduce(PageDown, paneId, initialState).state
    val updatedState = CursorViewport.ensureVisibleCursors(initialState, reducedState)
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(5, 0))
    buffer.viewport.topLine shouldBe 4
  }

  it should "move the cursor to the start of the file" in {
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
                .copy(content = com.serenity.rope.Rope("alpha\nbeta\ngamma")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(2, 3))),
              viewport = AppState.initial.persisted.buffers(bufferId).viewport.copy(topLine = 2)
            )
        )
      )
    )

    val reducedState = EditorEventReducer.reduce(MoveToStartOfFile, paneId, initialState).state
    val updatedState = CursorViewport.ensureVisibleCursors(initialState, reducedState)
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(0, 0))
    buffer.viewport.topLine shouldBe 0
  }

  it should "move the cursor to the end of the file" in {
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
                .copy(content = com.serenity.rope.Rope("alpha\nbeta\ngamma")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 1))),
              viewport = AppState.initial.persisted.buffers(bufferId).viewport.copy(visibleLines = 2)
            )
        )
      )
    )

    val reducedState = EditorEventReducer.reduce(MoveToEndOfFile, paneId, initialState).state
    val updatedState = CursorViewport.ensureVisibleCursors(initialState, reducedState)
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(2, 5))
    buffer.viewport.topLine shouldBe 1
  }

  it should "move the cursor down by a visible page" in {
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
                .copy(content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 1))),
              viewport = AppState.initial.persisted.buffers(bufferId).viewport.copy(visibleLines = 2)
            )
        )
      )
    )

    val reducedState = EditorEventReducer.reduce(PageDown, paneId, initialState).state
    val updatedState = CursorViewport.ensureVisibleCursors(initialState, reducedState)
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(3, 0))
    buffer.viewport.topLine shouldBe 2
  }

  it should "open the goto line modal from editor events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)

    val updatedState = EditorEventReducer.reduce(OpenGotoLine, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "provide a typed reducer instance for editor events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)
    val reducer      = EditorEventReducer.reducer(paneId)

    val updatedState = reducer.reduce(OpenGotoLine, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }
