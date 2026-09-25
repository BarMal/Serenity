package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Test viewport scrolling functionality to prevent text overflow beyond panel boundaries
  */
class ViewportScrollingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Viewport Horizontal Scrolling"

  it should "scroll right when cursor moves beyond visible columns" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val bufferId = stateManager.createBuffer("", None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager
      .updateState { current =>
        current.copy(persisted =
          current.persisted.copy(
            config = current.persisted.config.withWordWrap(false),
            buffers = current.persisted.buffers.updated(
              bufferId,
              current.persisted
                .buffers(bufferId)
                .copy(document = current.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
            )
          )
        )
      }
      .unsafeRunSync()

    // Get initial viewport settings
    val initialState   = stateManager.getCurrentState.unsafeRunSync()
    val initialPane    = initialState.persisted.layout.editorPanes(paneId)
    val visibleColumns = initialPane.viewport.visibleColumns

    // Insert text longer than visible area
    val longText = "x" * (visibleColumns + 5)
    longText.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane  = finalState.persisted.layout.editorPanes(paneId)
    val buffer     = finalPane.bufferId.flatMap(finalState.persisted.buffers.get).get
    val cursor     = buffer.editing.cursorPositions.head
    val viewport   = buffer.viewport

    // Cursor should be at end of text
    cursor.column shouldBe longText.length

    // Viewport should have scrolled to keep cursor visible
    cursor.column should be >= viewport.leftColumn
    cursor.column should be < (viewport.leftColumn + viewport.visibleColumns)

    // Viewport left column should be > 0 since we scrolled
    viewport.leftColumn should be > 0
  }

  it should "scroll left when cursor moves back to left edge" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val bufferId = stateManager.createBuffer("", None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager
      .updateState { current =>
        current.copy(persisted =
          current.persisted.copy(
            buffers = current.persisted.buffers.updated(
              bufferId,
              current.persisted
                .buffers(bufferId)
                .copy(document = current.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
            )
          )
        )
      }
      .unsafeRunSync()

    // Create long text and move cursor to the end
    val longText = "x" * 100
    longText.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Now move cursor back to the beginning. MoveToStartOfFile, not MoveToStart/Home: with wrap and
    // visual-line-navigation on (both default) and this 100-char run wrapping across multiple visual rows, Home now
    // lands on the cursor's current visual row rather than column 0 -- this test wants an unconditional jump to the
    // true start of the buffer to exercise the viewport scrolling back, not Home's own visual-row semantics.
    stateManager.applyEvent(MoveToStartOfFile).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane  = finalState.persisted.layout.editorPanes(paneId)
    val buffer     = finalPane.bufferId.flatMap(finalState.persisted.buffers.get).get
    val cursor     = buffer.editing.cursorPositions.head
    val viewport   = buffer.viewport

    // Cursor should be at column 0
    cursor.column shouldBe 0

    // Viewport should have scrolled back to show the beginning
    viewport.leftColumn shouldBe 0
  }

  behavior of "Viewport Vertical Scrolling"

  it should "scroll down when cursor moves beyond visible lines" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val bufferId = stateManager.createBuffer("", None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Get initial viewport settings
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val initialPane  = initialState.persisted.layout.editorPanes(paneId)
    val visibleLines = initialPane.viewport.visibleLines

    // Create text with more lines than visible
    val numLines = visibleLines + 5
    for _ <- 0 until numLines do
      stateManager.applyEvent(InsertChar('x')).unsafeRunSync()
      stateManager.applyEvent(NewLine).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane  = finalState.persisted.layout.editorPanes(paneId)
    val buffer     = finalPane.bufferId.flatMap(finalState.persisted.buffers.get).get
    val cursor     = buffer.editing.cursorPositions.head
    val viewport   = buffer.viewport

    // Cursor should be beyond the original visible area
    cursor.line should be >= visibleLines

    // Viewport should have scrolled to keep cursor visible
    cursor.line should be >= viewport.topLine
    cursor.line should be < (viewport.topLine + viewport.visibleLines)

    // Viewport top line should be > 0 since we scrolled
    viewport.topLine should be > 0
  }

  it should "scroll down when MoveDown carries the cursor beyond visible lines" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val bufferId = stateManager.createBuffer("", None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Get initial viewport settings
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val initialPane  = initialState.persisted.layout.editorPanes(paneId)
    val visibleLines = initialPane.viewport.visibleLines

    // Seed content with more lines than fit in the viewport, cursor left at the top.
    val numLines = visibleLines + 5
    for _ <- 0 until numLines do
      stateManager.applyEvent(InsertChar('x')).unsafeRunSync()
      stateManager.applyEvent(NewLine).unsafeRunSync()
    stateManager.applyEvent(MoveToStartOfFile).unsafeRunSync()

    // Sanity-check the baseline: cursor and viewport are both back at the top before any MoveDown runs, so a
    // pass below can only be explained by MoveDown itself scrolling the viewport -- not by scroll state NewLine
    // left behind earlier in this test.
    val afterMoveToStart = stateManager.getCurrentState.unsafeRunSync()
    val bufferAtStart    = afterMoveToStart.persisted.buffers(bufferId)
    bufferAtStart.editing.cursorPositions.head.line shouldBe 0
    bufferAtStart.viewport.topLine shouldBe 0

    // Walk the cursor down one line at a time via the arrow-key event, the same path a real terminal's Down key
    // takes (StateManagerEventPipeline's VerticalNavigationEvent branch), rather than NewLine's insertion path.
    for _ <- 0 until numLines do stateManager.applyEvent(MoveDown).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane  = finalState.persisted.layout.editorPanes(paneId)
    val buffer     = finalPane.bufferId.flatMap(finalState.persisted.buffers.get).get
    val cursor     = buffer.editing.cursorPositions.head
    val viewport   = buffer.viewport

    // Cursor should be beyond the original visible area
    cursor.line should be >= visibleLines

    // Viewport should have scrolled to keep the cursor visible
    cursor.line should be >= viewport.topLine
    cursor.line should be < (viewport.topLine + viewport.visibleLines)

    // Viewport top line should be > 0 since we scrolled
    viewport.topLine should be > 0
  }

  it should "scroll within a wrapped logical line to keep the cursor visible" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val bufferId = stateManager.createBuffer("", None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.paneManager.handleViewportResize(ViewportSize(32, 8)).unsafeRunSync()

    val text = List.fill(200)("word").mkString(" ")
    text.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.persisted.buffers(bufferId)
    val cursor     = buffer.editing.cursorPositions.head
    val font       = FontLoader.previewTextFont(finalState.persisted.config.editorConfig.fontConfig)
    val wrapPx =
      TextLayoutSnapshot.gridWrapWidthPx(
        buffer.viewport.visibleColumns,
        finalState.persisted.config.editorConfig.fontConfig
      )
    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, wrapPx, font)
    val cursorShown = snapshot.visualLines.exists(line =>
      line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn
    )

    withClue(
      s"viewport=${buffer.viewport} cursor=$cursor visualLines=${snapshot.visualLines.map(line => line.startColumn -> line.endColumn)}"
    ) {
      buffer.viewport.topVisualLine should be > 0
      cursorShown shouldBe true
    }
  }

  it should "keep wrapped text buffers anchored horizontally while navigating visual lines" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val bufferId = stateManager.createBuffer("", None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.paneManager.handleViewportResize(ViewportSize(32, 8)).unsafeRunSync()

    val text = List.fill(80)("wrapped").mkString(" ")
    text.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(MoveUp).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.persisted.buffers(bufferId)

    buffer.usesTextFont shouldBe true
    buffer.viewport.topVisualLine should be > 0
    buffer.viewport.leftColumn shouldBe 0
  }

  it should "keep the cursor visible across several wrapped paragraphs (visual-row aware recentring)" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Six paragraphs, each long enough to wrap across several visual rows at this width, followed by one short
    // final line -- several wrapped logical lines sit between the viewport's topLine and the cursor once it
    // lands on that short last line (cursor's own visual-row offset is small, so the buggy branch that subtracts
    // a visual-row count from a logical-line count is exercised). The buffer is seeded with all the content up
    // front (rather than typed in, which would self-correct the viewport one keystroke at a time) so a single
    // cursor jump exercises the recentring math directly.
    val paragraph  = List.fill(30)("word").mkString(" ")
    val paragraphs = List.fill(6)(paragraph) :+ "end"
    val bufferId   = stateManager.createBuffer(paragraphs.mkString("\n"), None).unsafeRunSync()
    val state      = stateManager.getCurrentState.unsafeRunSync()
    val paneId     = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.paneManager.handleViewportResize(ViewportSize(32, 8)).unsafeRunSync()

    stateManager.applyEvent(MoveToEndOfFile).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.persisted.buffers(bufferId)
    val cursor     = buffer.editing.cursorPositions.head
    val font       = FontLoader.previewTextFont(finalState.persisted.config.editorConfig.fontConfig)
    val wrapPx = TextLayoutSnapshot.gridWrapWidthPx(
      buffer.viewport.visibleColumns,
      finalState.persisted.config.editorConfig.fontConfig
    )
    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, wrapPx, font)
    val cursorShown = snapshot.visualLines.exists(line =>
      line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn
    )

    // Under the logical-line/visual-row conflation bug, the viewport under-scrolls once several wrapped lines
    // separate topLine from the cursor's line, leaving the cursor rendered past the bottom of the viewport.
    withClue(
      s"viewport=${buffer.viewport} cursor=$cursor visualLines=${snapshot.visualLines
          .map(line => line.bufferLine -> (line.startColumn, line.endColumn))}"
    ) {
      cursorShown shouldBe true
    }
  }

  it should "clamp the viewport at the document's end so no blank trailing rows are shown" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val bufferId = stateManager.createBuffer("", None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.paneManager.handleViewportResize(ViewportSize(32, 8)).unsafeRunSync()

    // Six wrapped paragraphs comfortably produce more total visual rows than the 8-row viewport, so once the
    // cursor lands at the very end, the viewport should be scrolled exactly to the document's end -- filling all
    // 8 rows with real content, not stopping short and leaving blank rows below the last line.
    val paragraph  = List.fill(30)("word").mkString(" ")
    val paragraphs = List.fill(6)(paragraph)
    paragraphs.zipWithIndex.foreach {
      case (text, index) =>
        text.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
        if index < paragraphs.length - 1 then stateManager.applyEvent(NewLine).unsafeRunSync()
    }

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.persisted.buffers(bufferId)
    val fontConfig = finalState.persisted.config.editorConfig.fontConfig
    val font       = FontLoader.previewTextFont(fontConfig)
    val wrapPx     = TextLayoutSnapshot.gridWrapWidthPx(buffer.viewport.visibleColumns, fontConfig)

    // `buffer.viewport.visibleLines` is a code-grid row count (`LayoutEngine.updateViewportDimensions` sets it from
    // the pane's grid rows) and is never itself resized for this buffer's own (proportional, Prose-role) font --
    // that resize only happens at render time (`RendererPaneSetup.snapshotForBuffer`'s
    // `visibleLines = panelHeightPx / bufferMetrics.lineHeight`) and when `CursorViewport.adjustForCursor` placed
    // this buffer's `topLine`/`topVisualLine` after the cursor moved (its own, equivalent `effectiveVisibleLines`).
    // Calling `fromBuffer` on the raw, un-resized viewport -- as this test did before -- asks it for the code-grid
    // row count from a position CursorViewport computed for a *different* (font-adjusted) row count; the two only
    // coincide when the buffer's own font happens to share the code font's line height, which is not guaranteed
    // across platforms. Mirroring the same resize here measures what production actually renders (#1041's
    // `effectiveVisibleLines`), not an incidental platform coincidence.
    val codeLineHeightPx =
      CellMetrics.fromFont(FontLoader.previewFontForRole(fontConfig, TypographyRole.Code)).lineHeight
    val panelHeightPx = buffer.viewport.visibleLines * codeLineHeightPx
    val renderedVisibleLines =
      math.max(1, panelHeightPx / math.max(1, CellMetrics.fromFont(font).lineHeight))
    val renderBuffer = buffer.copy(viewport = buffer.viewport.copy(visibleLines = renderedVisibleLines))

    val snapshot = TextLayoutSnapshot.fromBuffer(renderBuffer, wrapPx, font)

    // How many "word" tokens fit per wrapped row -- and so how many total visual rows the six paragraphs produce --
    // is font-metric-dependent (word wrap is measured against the real, platform-resolved `previewTextFont`, not a
    // fixed cell grid). Asserting a specific row count or "the last visual line is buffer line N" against a number
    // computed by hand would be re-deriving that same font-dependent arithmetic by inspection and baking today's
    // platform's answer into the test. Instead, lay out the *entire* document with the identical wrap computation
    // the production snapshot uses, and check that what's on screen is exactly that layout's tail -- which is what
    // "scrolled to the document's end, no blank trailing rows" actually means, regardless of how many rows the
    // resolved font happens to wrap the text into.
    val fullDocumentSnapshot = TextLayoutSnapshot.fromBuffer(
      renderBuffer.copy(viewport =
        renderBuffer.viewport.copy(topLine = 0, topVisualLine = 0, visibleLines = Int.MaxValue - 1)
      ),
      wrapPx,
      font
    )
    val expectedVisibleCount    = math.min(renderedVisibleLines, fullDocumentSnapshot.visualLines.length)
    val expectedTailVisualLines = fullDocumentSnapshot.visualLines.takeRight(expectedVisibleCount)
    def shape(line: com.serenity.state.models.TextVisualLine) = (line.bufferLine, line.startColumn, line.endColumn)

    withClue(
      s"viewport=${buffer.viewport} renderedVisibleLines=$renderedVisibleLines " +
        s"snapshot=${snapshot.visualLines.map(shape)} " +
        s"expectedTail=${expectedTailVisualLines.map(shape)} " +
        s"totalVisualLines=${fullDocumentSnapshot.visualLines.length}"
    ) {
      snapshot.visualLines.length shouldBe expectedVisibleCount
      snapshot.visualLines.map(shape) shouldBe expectedTailVisualLines.map(shape)
    }
  }

  behavior of "Buffer Content Integrity"

  it should "preserve all text content regardless of viewport position" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val bufferId = stateManager.createBuffer("", None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Create content that will require both horizontal and vertical scrolling
    val testContent = "The quick brown fox jumps over the lazy dog. " * 10 + "\n"
    val numLines    = 30

    for _ <- 0 until numLines do
      testContent.foreach { char =>
        if char == '\n' then stateManager.applyEvent(NewLine).unsafeRunSync()
        else stateManager.applyEvent(InsertChar(char)).unsafeRunSync()
      }

    val finalState      = stateManager.getCurrentState.unsafeRunSync()
    val buffer          = finalState.persisted.buffers(bufferId)
    val actualContent   = buffer.document.content.collect()
    val expectedContent = testContent * numLines

    // All content should be preserved exactly
    actualContent shouldBe expectedContent

    // Content should be significantly larger than any viewport
    actualContent.length should be > 1000
    actualContent.count(_ == '\n') shouldBe numLines
  }

  it should "handle rapid scrolling movements without losing content" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    val bufferId = stateManager.createBuffer("Initial text", None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Perform rapid movements that would trigger viewport changes
    val movements = List(
      MoveToEnd, // Move to end of initial text
      InsertChar('!'),
      InsertChar('!'),
      InsertChar('!'), // Add more text
      NewLine,         // New line
      InsertChar('N'),
      InsertChar('e'),
      InsertChar('w'), // Add "New line"
      MoveToStart,     // Back to beginning
      InsertChar('S'),
      InsertChar('t'),
      InsertChar('a'),
      InsertChar('r'),
      InsertChar('t'), // Add "Start"
      MoveToEnd        // Back to end
    )

    movements.foreach(event => stateManager.applyEvent(event).unsafeRunSync())

    val finalState   = stateManager.getCurrentState.unsafeRunSync()
    val buffer       = finalState.persisted.buffers(bufferId)
    val finalContent = buffer.document.content.collect()

    // Content should contain all our additions
    finalContent should include("Initial text")
    finalContent should include("Start")
    finalContent should include("New")
    finalContent should include("!!!")

    // Should have exactly one newline
    finalContent.count(_ == '\n') shouldBe 1
  }
