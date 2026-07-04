package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{PinnedPanelViewModel, Renderer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class MarkdownViewModeSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("MarkdownViewModeSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    val beforeOpen = stateManager.getCurrentState.unsafeRunSync()
    if beforeOpen.commandRunnerSurface
          .flatMap {
            _.content match
              case SurfaceContent.CommandPalette(runner) => Some(runner.isActive)
              case _                                     => None
          }
          .getOrElse(false) == false
    then stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)
    stateManager.applyEvent(Enter).unsafeRunSync()

  private def markdownEditorState(mode: MarkdownViewMode): AppState =
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# Rendered\n\n# Raw\ncontinued")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(2, 0)),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppConfig.default
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(mode)
    )

  "Markdown view mode settings" should "default to source editing" in {
    AppConfig.default.markdownViewMode shouldBe MarkdownViewMode.Source
  }

  it should "include a command runner settings option for markdown view mode" in {
    val runner = CommandRunner.empty.activate(
      com.serenity.command.CommandRegistry.withToggleUI,
      AppConfig.default.withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )

    def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
      group.children.flatMap {
        case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
        case child                               => List(child)
      }

    val markdownItem = runner.settingsGroups
      .flatMap(group => group :: descendants(group))
      .collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == "markdown-view" => item }

    markdownItem.map(_.selectedOption) shouldBe Some("Inline Lens")
    markdownItem.map(_.options.map(_.label)) shouldBe Some(List("Source", "Split Preview", "Inline Lens"))
  }

  it should "switch to split preview mode and pin a live markdown preview panel" in {
    val stateManager = createStateManager()

    stateManager
      .updateState { state =>
        val bufferId = BufferId(0)
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = Rope("# Notes\n\nInitial text"),
            language = Some(LanguageId.Markdown)
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "markdown-view-split", "markdown-view-split")

    val splitState = stateManager.getCurrentState.unsafeRunSync()
    splitState.config.markdownViewMode shouldBe MarkdownViewMode.SplitPreview
    splitState.pinnedSurfaces.collectFirst {
      case UiSurface(
            _,
            SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"),
            SurfacePresentation.Pinned(PanelPosition.Right, 40),
            _
          ) =>
        true
    } shouldBe Some(true)

    stateManager
      .updateState { state =>
        val bufferId = BufferId(0)
        val updated  = state.buffers(bufferId).copy(content = Rope("# Notes\n\nUpdated live text"))
        state.copy(buffers = state.buffers + (bufferId -> updated))
      }
      .unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val layout       = LayoutEngine.calculateLayout(updatedState, ViewportSize(100, 24))
    val rightPanel = PinnedPanelViewModel
      .fromState(updatedState, layout)
      .find(_.title == "Preview: Untitled")

    rightPanel.map(_.lines.exists(_.contains("Notes"))) shouldBe Some(true)
    rightPanel.map(_.lines.exists(_.contains("Updated live text"))) shouldBe Some(true)
  }

  it should "remove only markdown preview panels when leaving split preview mode" in {
    val stateManager = createStateManager()

    stateManager
      .updateState { state =>
        state.copy(
          uiSurfaces = state.uiSurfaces ++ List(
            UiSurface(
              SurfaceId("outline"),
              SurfaceContent.Outline(Nil),
              SurfacePresentation.Pinned(PanelPosition.Right, 30)
            ),
            UiSurface(
              SurfaceId("markdown-preview"),
              SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"),
              SurfacePresentation.Pinned(PanelPosition.Right, 40)
            )
          ),
          config = state.config.withMarkdownViewMode(MarkdownViewMode.SplitPreview),
          focus = Focus.Surface(SurfaceId("outline"))
        )
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "markdown-view-source",
          "Switch Markdown rendering back to source mode.",
          CommandIntent.SetMarkdownViewMode(MarkdownViewMode.Source),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.config.markdownViewMode shouldBe MarkdownViewMode.Source
    state.pinnedSurfaces.map(_.id) should contain only SurfaceId("outline")
    state.focus shouldBe Focus.Surface(SurfaceId("outline"))
  }

  it should "render split previews as a Java2D markdown image" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val state = AppState.empty.copy(
      buffers = Map(
        bufferId -> Buffer
          .fromString(bufferId, "# Notes\n\n| Task | Owner |\n| ---- | ----- |\n| Ship | Codex |")
          .copy(language = Some(LanguageId.Markdown))
      ),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("markdown-preview"),
          SurfaceContent.MarkdownPreview(bufferId, "notes.md"),
          SurfacePresentation.Pinned(PanelPosition.Right, 40)
        )
      ),
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.SplitPreview)
    )
    val surface = new MockRenderSurface(120, 32)
    val font    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(120, 32),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    surface.drawImageCalls should have size 1
    val drawn = surface.drawImageCalls.head
    drawn.image.getWidth shouldBe drawn.width * CellMetrics.fromFont(font).charWidth
    drawn.image.getHeight shouldBe drawn.height * CellMetrics.fromFont(font).lineHeight
    surfaceRows(surface).exists(_.contains("Task  Owner")) shouldBe false
  }

  it should "update the split preview image when the source cursor moves outside the current preview window" in {
    val source = (1 to 200).map(i => s"# Heading $i").mkString("\n")

    val firstImage = renderSplitPreviewImage(
      markdownPreviewPanelState(source, CursorPosition(0, 0))
    )
    val laterImage = renderSplitPreviewImage(
      markdownPreviewPanelState(source, CursorPosition(180, 0))
    )

    laterImage should not be theSameInstanceAs(firstImage)
  }

  "Markdown inline lens mode" should "leave markdown source untouched in source mode" in {
    val surface = new MockRenderSurface(100, 20)
    val font    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    Renderer.render(
      markdownEditorState(MarkdownViewMode.Source),
      cursorVisible = true,
      surface,
      ViewportSize(100, 20),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    val rows = surfaceRows(surface)
    rows.exists(_.contains("# Rendered")) shouldBe true
    rows.exists(_.contains("# Raw")) shouldBe true
  }

  it should "render markdown preview as an image and overlay the active block as raw source" in {
    val surface = new MockRenderSurface(100, 20)
    val font    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    Renderer.render(
      markdownEditorState(MarkdownViewMode.InlineLens),
      cursorVisible = true,
      surface,
      ViewportSize(100, 20),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    val rows = surfaceRows(surface)
    surface.drawImageCalls should have size 1
    rows.exists(_.contains("# Rendered")) shouldBe false
    rows.exists(_.contains("# Raw")) shouldBe true
    rows.exists(_.contains("continued")) shouldBe false
  }

  it should "render inactive markdown tables through the markdown preview image in inline lens mode" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val state = AppState.empty.copy(
      buffers = Map(
        bufferId -> Buffer
          .fromString(
            bufferId,
            """|| Task | Owner |
              || ---- | ----- |
              || Ship | Codex |
              |
              |Editing here""".stripMargin
          )
          .copy(
            language = Some(LanguageId.Markdown),
            cursors = List(CursorPosition(4, 0)),
            viewport = Viewport.default.copy(visibleLines = 10)
          )
      ),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppConfig.default
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(100, 20)
    val font    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(100, 20),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    surface.drawImageCalls should have size 1
    surfaceRows(surface).exists(_.contains("| ---- | ----- |")) shouldBe false
  }

  private def surfaceRows(surface: MockRenderSurface): List[String] =
    (0 until surface.height).map(surface.getRow).map(_.trim).filter(_.nonEmpty).toList

  private def markdownPreviewPanelState(source: String, cursor: CursorPosition): AppState =
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    AppState.empty.copy(
      buffers = Map(
        bufferId -> Buffer
          .fromString(bufferId, source)
          .copy(
            language = Some(LanguageId.Markdown),
            cursors = List(cursor),
            viewport = Viewport.default.copy(visibleLines = 10)
          )
      ),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("markdown-preview"),
          SurfaceContent.MarkdownPreview(bufferId, "notes.md"),
          SurfacePresentation.Pinned(PanelPosition.Right, 40)
        )
      ),
      focus = Focus.EditorPane(paneId),
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.SplitPreview)
    )

  private def renderSplitPreviewImage(state: AppState): java.awt.image.BufferedImage =
    val surface = new MockRenderSurface(120, 32)
    val font    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(120, 32),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    surface.drawImageCalls.headOption.map(_.image).getOrElse(fail("Expected rendered markdown preview image"))

end MarkdownViewModeSpec
