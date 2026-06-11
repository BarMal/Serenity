package com.serenity

import java.awt.Font

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import com.serenity.app.AppStartup
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StartupRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Startup State Rendering"

  it should "render the dedicated start page vertically centered in the viewport" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      state        <- AppStartup.startPageState(stateManager, com.serenity.ui.theme.Theme.dark, ViewportSize(100, 30))
    yield
      val surface = new MockRenderSurface(100, 30)
      Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

      val renderedLines =
        (0 until 30).flatMap { y =>
          val line = (0 until 100).map(x => surface.getChar(x, y)).mkString.trim
          Option.when(line.nonEmpty)((y, line))
        }

      val startPage         = state.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      val fullExpectedLines = startPage.renderLines
      val expectedLines     = fullExpectedLines.filter(_.nonEmpty)
      val expectedStartY    = (30 - fullExpectedLines.size) / 2

      renderedLines.map(_._2) should contain allElementsOf expectedLines

      renderedLines should contain((expectedStartY, startPage.title))
      renderedLines should contain((expectedStartY + 2, startPage.options.head))

    program.unsafeRunSync()
  }

  it should "center start page text by measured UI font width" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      state        <- AppStartup.startPageState(stateManager, com.serenity.ui.theme.Theme.dark, ViewportSize(100, 30))
    yield
      val surface     = new MockRenderSurface(100, 30)
      val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
      val uiFont      = Font(Font.SERIF, Font.PLAIN, 12)
      val codeMetrics = CellMetrics.fromFont(codeFont)
      val uiMetrics   = CellMetrics.fromFont(uiFont)

      Renderer.render(
        state,
        cursorVisible = true,
        surface,
        ViewportSize(100, 30),
        codeFont,
        codeFont,
        uiFont,
        codeMetrics,
        uiMetrics,
        None
      )

      val title    = state.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page.title
      val titleRun = surface.drawRunPxCalls.find(_.s == title).getOrElse(fail("expected measured title draw call"))
      val textWidth =
        uiFont.getStringBounds(title, surface.fontRenderContext.getOrElse(fail("missing font render context"))).getWidth
      val expectedX = ((100 * codeMetrics.charWidth) - textWidth) / 2.0

      titleRun.xPx.toDouble shouldBe expectedX +- 1.0

    program.unsafeRunSync()
  }

  it should "center blank buffer text by measured text font width" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "")
      .copy(isNewEmpty = false, viewport = Viewport.default.copy(visibleLines = 10))
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config.withLineNumbers(false).withGutter(false)
    )
    val surface     = new MockRenderSurface(80, 24)
    val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val textFont    = Font(Font.SERIF, Font.PLAIN, 12)
    val codeMetrics = CellMetrics.fromFont(codeFont)
    val textMetrics = CellMetrics.fromFont(textFont)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = codeFont,
      textFont = textFont,
      cellMetrics = codeMetrics,
      cursorColor = None
    )

    val paneRect =
      com.serenity.ui.layout.LayoutEngine.calculatePaneLayouts(
        state,
        com.serenity.ui.layout.LayoutEngine.calculateLayout(state, ViewportSize(80, 24))
      )(paneId)
    val contentRect = paneRect.copy(y = paneRect.y + 1, height = math.max(1, paneRect.height - 1))
    val expected = TextAlignment.placeLine(
      "~ Empty ~",
      com.serenity.ui.layout.TextAreaPx(
        codeMetrics.toPixelX(contentRect.x).toFloat,
        codeMetrics.toPixelY(contentRect.y + contentRect.height / 2),
        contentRect.width * codeMetrics.charWidth,
        codeMetrics.lineHeight
      ),
      textFont,
      codeMetrics.lineHeight,
      textMetrics.ascent,
      com.serenity.ui.layout.TextHorizontalAlignment.Center,
      com.serenity.ui.layout.TextVerticalAlignment.Top,
      surface.fontRenderContext.get
    )

    val call = surface.drawRunPxCalls.find(_.s == "~ Empty ~").getOrElse(fail("expected empty text draw call"))
    call.xPx shouldBe expected.xPx +- 0.001f
  }

  it should "have buffer content available immediately after setup" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("Welcome to Serenity!")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _          <- stateManager.setBufferForPane(paneId, bufferId)
      finalState <- stateManager.getCurrentState
    yield
      val buffer = finalState.buffers(bufferId)
      buffer.content.collect() shouldBe "Welcome to Serenity!"
      val pane = finalState.layout.editorPanes(paneId)
      pane.bufferId shouldBe Some(bufferId)

    program.unsafeRunSync()
  }

  it should "have proper cursor position in initial state" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("Hello")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _          <- stateManager.setBufferForPane(paneId, bufferId)
      finalState <- stateManager.getCurrentState
    yield
      val buffer = finalState.buffers(bufferId)
      buffer.cursors.size shouldBe 1
      val cursor = buffer.cursors.head
      cursor.line shouldBe 0
      cursor.column shouldBe 0

    program.unsafeRunSync()
  }

  behavior of "Text Overflow Prevention"

  it should "properly track cursor position when text extends beyond typical panel width" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)
      longText = "x" * 100
      _          <- longText.toList.traverse(char => stateManager.applyEvent(InsertChar(char)))
      finalState <- stateManager.getCurrentState
    yield
      val buffer = finalState.buffers(bufferId)
      val cursor = buffer.cursors.head
      cursor.column shouldBe 100
      cursor.line shouldBe 0
      buffer.content.collect() shouldBe longText

    program.unsafeRunSync()
  }

  it should "handle viewport scrolling when cursor extends beyond visible area" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)
      panelWidth = 50
      longText   = "a" * (panelWidth + 20)
      _          <- longText.toList.traverse(char => stateManager.applyEvent(InsertChar(char)))
      finalState <- stateManager.getCurrentState
    yield
      val buffer   = finalState.buffers(bufferId)
      val cursor   = buffer.cursors.head
      val viewport = buffer.viewport
      cursor.column shouldBe longText.length
      info(
        s"Cursor at column ${cursor.column}, viewport left=${viewport.leftColumn}, visible=${viewport.visibleColumns}"
      )

    program.unsafeRunSync()
  }

  it should "preserve buffer content integrity regardless of viewport scrolling" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      bufferId     <- stateManager.createBuffer("")
      state        <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)
      testText =
        "The quick brown fox jumps over the lazy dog. This is a long sentence that extends beyond normal panel boundaries."
      _          <- testText.toList.traverse(char => stateManager.applyEvent(InsertChar(char)))
      finalState <- stateManager.getCurrentState
    yield
      val buffer = finalState.buffers(bufferId)
      buffer.content.collect() shouldBe testText

    program.unsafeRunSync()
  }
