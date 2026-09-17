package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.TextAreaInsets
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class MouseClickCursorSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  // Layout at 80x24, showLineNumbers=true, showGutter=true:
  //   gutterHeight=1  → contentHeight=23
  //   spacerWidth=(80*0.15).toInt=12
  //   lineNumberWidth for 4-line buffer = max(3, "4".length+1) = 3
  //   editorPanelRect = LayoutRect(x=15, y=0, width=53, height=23)
  //   PaneId(0) → LayoutRect(x=15, y=0, width=53, height=23)
  //   contentRow starts at y=1 (header at y=0)
  //
  // `makeStateManager()` seeds `AppState.initial`'s own buffer, and every test below adds a *second* one via
  // `createBuffer` without closing the first -- so every fixture here has 2 buffers open, reserving the
  // always-visible tab bar's row (issue #1074/#1075/#1076/#1077) above everything the comment above describes:
  // row 0 is the tab bar, row 1 is the pane header, and content starts at row 2 -- one row lower than this file's
  // original geometry notes, everywhere a click's row is asserted below.

  "MouseClick" should "move cursor to the clicked buffer position" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("aaaa\nbbbb\ncccc\ndddd", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // Click at row 4: content now starts at row 2 (tab bar at row 0, pane header at row 1), so bufferLine =
    // topLine(0) + (4-2) = 2, bufferCol = leftCol(0) + 3 = 3.
    sm.applyEvent(MouseClick(6, 4)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption.map(_.line) shouldBe Some(2)
    buffer.editing.cursors.headOption.map(_.column) shouldBe Some(3)
  }

  it should "move cursor to the first row of the content area" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hello\nworld", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // First cell of the content area is now row 2 (tab bar at row 0, pane header at row 1) → bufferLine=0, bufferCol=0
    sm.applyEvent(MouseClick(3, 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption.map(_.line) shouldBe Some(0)
    buffer.editing.cursors.headOption.map(_.column) shouldBe Some(0)
  }

  it should "not move the cursor for non-primary clicks" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hello\nworld", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(
                document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)),
                editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 1)))
              )
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    sm.applyEvent(MouseClick(18, 2, button = MouseButton.Secondary)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors shouldBe List(CursorPosition(0, 1))
  }

  it should "clamp column to line length when clicking past end of line" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hi\nworld", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // Row 2 is the content area's first row (tab bar at row 0, pane header at row 1): bufferLine=0,
    // bufferCol = 35-15 = 20, "hi" length=2 → clamp to 2
    sm.applyEvent(MouseClick(35, 2)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption.map(_.line) shouldBe Some(0)
    buffer.editing.cursors.headOption.map(_.column) shouldBe Some(2)
  }

  it should "track the editor position under the pointer on mouse move" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hello\nworld", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    // Row 3 is content row 1 now that content starts at row 2 (tab bar at row 0, pane header at row 1).
    sm.applyEvent(MouseMove(6, 3)).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().runtime.hoveredEditorTarget shouldBe Some(
      HoveredEditorTarget(PaneId(0), bufferId, CursorPosition(1, 3))
    )
  }

  it should "clear the editor hover target when the pointer leaves editor panes" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hello\nworld", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    sm.applyEvent(MouseMove(6, 2)).unsafeRunSync()
    sm.applyEvent(MouseMove(0, 23)).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().runtime.hoveredEditorTarget shouldBe None
  }

  it should "ignore clicks in the pane header row" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hello", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val initialCursor = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.headOption

    // Click at row=0 (header row of pane at y=0) — should be ignored
    sm.applyEvent(MouseClick(20, 0)).unsafeRunSync()

    val afterCursor = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.headOption
    afterCursor shouldBe initialCursor
  }

  it should "ignore clicks in the spacer area outside any pane" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hello", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState(state =>
      state.copy(persisted =
        state.persisted.copy(config = state.persisted.config.withTextAreaInsets(TextAreaInsets(0.15, 0.15)))
      )
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val initialCursor = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.headOption

    // Click at col=5 (left spacer, pane starts at col=15) — should be ignored
    sm.applyEvent(MouseClick(5, 5)).unsafeRunSync()

    val afterCursor = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.headOption
    afterCursor shouldBe initialCursor
  }

  it should "ignore clicks when terminal size has not been set" in {
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("hello", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    // No ResizeEvent applied — ViewportSize is None

    val initialCursor = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.headOption

    sm.applyEvent(MouseClick(20, 5)).unsafeRunSync()

    val afterCursor = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.headOption
    afterCursor shouldBe initialCursor
  }

  it should "use pixel-aware hit testing for proportional text when pixel coordinates are available" in {
    given LoggerFactory[IO]                 = Slf4jFactory.create[IO]
    given org.typelevel.log4cats.Logger[IO] = org.typelevel.log4cats.slf4j.Slf4jLogger.getLogger[IO]

    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("iW", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Markdown)))
          )
        )
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()

    val state       = sm.getCurrentState.unsafeRunSync()
    val layout      = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))
    val contentRect = CursorLayout.contentRectForPane(paneRect)
    val font = FontLoader.previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Prose)
    // Mouse pixels arrive in the screen grid's coordinates, and that grid is the code font's even for prose.
    val gridMetrics =
      CellMetrics.fromFont(
        FontLoader.previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Code)
      )
    val panelWidthPx = contentRect.width * gridMetrics.charWidth
    val snapshot     = TextLayoutSnapshot.fromBuffer(state.persisted.buffers(bufferId), panelWidthPx, font)
    val line         = snapshot.visualLines.head
    val pixelX       = contentRect.x * gridMetrics.charWidth + math.round(line.xForColumn(1).getOrElse(0.0f) + 1.0f)
    val pixelY       = contentRect.y * gridMetrics.lineHeight

    sm.applyEvent(
      MouseClick(
        col = contentRect.x,
        row = contentRect.y,
        pixelX = Some(pixelX),
        pixelY = Some(pixelY)
      )
    ).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(com.serenity.state.models.CursorPosition(0, 1))
  }

  it should "resolve a click below wrapped text to the last visual row of the wrap group, not the first" in {
    // #1547: with line wrap on, clicking beneath the last visible row must land on the LAST visual row of
    // whichever logical line the click falls back to, not that line's first (unwrapped-looking) row -- the
    // fallback in EditorMouseTargeting.resolveMouseTarget must not treat the click's column as a raw offset
    // into the logical line (which is only correct for that line's first wrap segment).
    val sm       = makeStateManager()
    val bufferId = sm.bufferManager.createBuffer("placeholder", None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(ViewportSize(80, 24))).unsafeRunSync()
    sm.updateState(state => state.copy(runtime = state.runtime.copy(isTuiMode = true))).unsafeRunSync()

    val state       = sm.getCurrentState.unsafeRunSync()
    val layout      = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    // One logical line, long enough to wrap into three visual rows on the cell grid: two full-width rows plus a
    // short third row. The click below lands on that third row's column range.
    val fullRowLength      = contentRect.width
    val lastRowLength      = 5
    val lineText           = "x" * (fullRowLength * 2 + lastRowLength)
    val lastRowStartColumn = fullRowLength * 2
    val xOffsetOnLastRow   = 3

    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(buffers =
          state.persisted.buffers.updated(
            bufferId,
            state.persisted
              .buffers(bufferId)
              .copy(document =
                state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope(lineText))
              )
          )
        )
      )
    }.unsafeRunSync()

    // Click a row below all three wrapped rows (still inside the content area) at a column that is only valid
    // relative to the last wrapped row's own start.
    val clickRow = contentRect.y + 5
    val clickCol = contentRect.x + xOffsetOnLastRow
    sm.applyEvent(MouseClick(clickCol, clickRow)).unsafeRunSync()

    val buffer = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    buffer.editing.cursors.headOption shouldBe Some(
      com.serenity.state.models.CursorPosition(0, lastRowStartColumn + xOffsetOnLastRow)
    )
  }
