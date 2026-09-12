package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfigMotionOps.*
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

    // Click at (18, 3): bufferLine = topLine(0) + (3-1) = 2, bufferCol = leftCol(0) + (18-15) = 3
    sm.applyEvent(MouseClick(6, 3)).unsafeRunSync()

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

    // Click at (15, 1): first cell of content area → bufferLine=0, bufferCol=0
    sm.applyEvent(MouseClick(3, 1)).unsafeRunSync()

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

    // Click at (35, 1): bufferLine=0, bufferCol = 35-15 = 20, "hi" length=2 → clamp to 2
    sm.applyEvent(MouseClick(35, 1)).unsafeRunSync()

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

    sm.applyEvent(MouseMove(6, 2)).unsafeRunSync()

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

