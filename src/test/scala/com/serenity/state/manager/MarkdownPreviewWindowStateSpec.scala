package com.serenity.state.manager

import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, ViewIntent}
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.tui.{MarkdownPreviewWindow, MarkdownPreviewWindowAvailability}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Reducer/state-management coverage for issue #1113's TUI Markdown preview window toggle -- the JFrame shell itself is
  * verified manually (see the PR description's checklist); everything below exercises `ViewIntent
  * .OpenMarkdownPreview`'s TUI branch purely through the public `StateManager`/`AppState` surface, using a fake
  * [[MarkdownPreviewWindow]] double so no AWT/Swing type is ever touched.
  */
class MarkdownPreviewWindowStateSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default
  given LoggerFactory[IO]         = Slf4jFactory.create[IO]

  final private class FakeWindow extends MarkdownPreviewWindow:
    val showCount = new AtomicInteger(0)
    val hideCount = new AtomicInteger(0)

    def show(): IO[Unit]                            = IO(showCount.incrementAndGet()).void
    def hide(): IO[Unit]                            = IO(hideCount.incrementAndGet()).void
    def updateImage(image: BufferedImage): IO[Unit] = IO.unit
    def currentSize: IO[(Int, Int)]                 = IO.pure((640, 800))
    def setOnUserClose(callback: () => Unit): Unit  = ()

  private def createStateManager(
    markdownPreviewWindow: MarkdownPreviewWindowAvailability
  ): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("MarkdownPreviewWindowStateSpec"))
    StateManager.apply(logger, markdownPreviewWindow = markdownPreviewWindow).unsafeRunSync()

  private val bufferId = BufferId(1)
  private val paneId   = PaneId(1)

  private def markdownBufferState(isTuiMode: Boolean): AppState =
    val baseBuffer = Buffer.fromString(bufferId, "# Hello\n\nSome text")
    val buffer     = baseBuffer.copy(document = baseBuffer.document.copy(language = Some(LanguageId.Markdown)))
    AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = AppState.empty.runtime.copy(isTuiMode = isTuiMode)
    )

  private val openMarkdownPreview =
    Command.typed(
      "markdown-preview-open",
      "Open Markdown preview",
      CommandIntent.View(ViewIntent.OpenMarkdownPreview),
      CommandCategory.View
    )

  "OpenMarkdownPreview in TUI mode" should "report unavailability via a peek instead of opening a window when no display was reachable" in {
    val stateManager = createStateManager(MarkdownPreviewWindowAvailability.Unavailable)
    stateManager.updateState(_ => markdownBufferState(isTuiMode = true)).unsafeRunSync()

    stateManager.executeCommand(openMarkdownPreview).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.runtime.markdownPreviewWindowBuffer shouldBe None
    state.runtime.uiSurfaces.exists {
      case UiSurface(_, SurfaceContent.QuickInfo(text), _, _) => text.contains("graphical display")
      case _                                                  => false
    } shouldBe true
  }

  it should "open the window for the focused Markdown buffer and record it in state" in {
    val window       = new FakeWindow
    val stateManager = createStateManager(MarkdownPreviewWindowAvailability.Available(window))
    stateManager.updateState(_ => markdownBufferState(isTuiMode = true)).unsafeRunSync()

    stateManager.executeCommand(openMarkdownPreview).unsafeRunSync()

    window.showCount.get() shouldBe 1
    stateManager.getCurrentState.unsafeRunSync().runtime.markdownPreviewWindowBuffer shouldBe Some(bufferId)
  }

  it should "close the window and clear state on a second toggle" in {
    val window       = new FakeWindow
    val stateManager = createStateManager(MarkdownPreviewWindowAvailability.Available(window))
    stateManager.updateState(_ => markdownBufferState(isTuiMode = true)).unsafeRunSync()

    stateManager.executeCommand(openMarkdownPreview).unsafeRunSync()
    stateManager.executeCommand(openMarkdownPreview).unsafeRunSync()

    window.showCount.get() shouldBe 1
    window.hideCount.get() shouldBe 1
    stateManager.getCurrentState.unsafeRunSync().runtime.markdownPreviewWindowBuffer shouldBe None
  }

  it should "report unavailability without opening a window when there is no active Markdown buffer" in {
    val window       = new FakeWindow
    val stateManager = createStateManager(MarkdownPreviewWindowAvailability.Available(window))
    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(isTuiMode = true)))
      .unsafeRunSync()

    stateManager.executeCommand(openMarkdownPreview).unsafeRunSync()

    window.showCount.get() shouldBe 0
    stateManager.getCurrentState.unsafeRunSync().runtime.markdownPreviewWindowBuffer shouldBe None
  }

  "OpenMarkdownPreview in GUI mode" should "still pin the in-app preview panel, unaffected by the TUI window plumbing" in {
    val stateManager = createStateManager(MarkdownPreviewWindowAvailability.Unavailable)
    stateManager.updateState(_ => markdownBufferState(isTuiMode = false)).unsafeRunSync()

    stateManager.executeCommand(openMarkdownPreview).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.runtime.markdownPreviewWindowBuffer shouldBe None
    state.runtime.uiSurfaces.exists {
      case UiSurface(_, SurfaceContent.MarkdownPreview(id, _), _, _) => id == bufferId
      case _                                                         => false
    } shouldBe true
  }
