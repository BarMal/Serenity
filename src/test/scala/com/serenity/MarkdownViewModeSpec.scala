package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{FontSpec, PinnedPanelViewModel, RendererEntryPoints}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class MarkdownViewModeSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("MarkdownViewModeSpec"))
    StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()

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
    val bufferId   = BufferId(1)
    val paneId     = PaneId(1)
    val baseBuffer = Buffer.fromString(bufferId, "# Rendered\n\n# Raw\ncontinued")
    val buffer = baseBuffer
      .copy(
        document = baseBuffer.document.copy(language = Some(LanguageId.Markdown)),
        editing = EditingState(List(CursorPosition(2, 0))),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppConfig.default
          .withSyntaxHighlighting(true)
          .withLineNumbers(false)
          .withoutStatusLine
          .withMarkdownViewMode(mode)
      )
    )

  "Markdown view mode settings" should "default to source editing" in {
    AppConfig.default.markdownViewMode shouldBe MarkdownViewMode.Source
  }

  it should "store markdown and default document settings inside the document sub-config" in {
    val config = AppConfig.default
      .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      .withDefaultDocumentMode(DefaultDocumentMode.RichText)

    config.documentConfig shouldBe DocumentConfig(
      markdownViewMode = MarkdownViewMode.InlineLens,
      defaultMode = DefaultDocumentMode.RichText
    )
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
    markdownItem.map(_.options.map(_.label)) shouldBe Some(
      List("Source", "Split Preview", "Inline Lens", "Live Preview", "Read")
    )
  }

  it should "switch to split preview mode and pin a live markdown preview panel" in {
    val stateManager = createStateManager()

    stateManager
      .updateState { state =>
        val bufferId = BufferId(0)
        val existing = state.persisted.buffers(bufferId)
        val buffer = existing
          .copy(
            document = existing.document.copy(
              content = Rope("# Notes\n\nInitial text"),
              language = Some(LanguageId.Markdown)
            )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "markdown-view",
          "Markdown View",
          CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.SplitPreview)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val splitState = stateManager.getCurrentState.unsafeRunSync()
    splitState.persisted.config.markdownViewMode shouldBe MarkdownViewMode.SplitPreview
    splitState.pinnedSurfaces.collectFirst {
      case surface @ UiSurface(_, SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"), _, _)
          if splitState.persisted.layout.workspaceTree
            .flatMap(_.positionForSurface(surface.id))
            .contains(
              PanelPosition.Right
            ) =>
        true
    } shouldBe Some(true)

    stateManager
      .updateState { state =>
        val bufferId = BufferId(0)
        val existing = state.persisted.buffers(bufferId)
        val updated =
          existing.copy(document = existing.document.copy(content = Rope("# Notes\n\nUpdated live text")))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updated)))
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
        val docked = DockedPanelFixtures.dockAllContent(
          state,
          List(
            (SurfaceId("outline"), SurfaceContent.Outline(Nil), PanelPosition.Right, 30),
            (
              SurfaceId("markdown-preview"),
              SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"),
              PanelPosition.Right,
              40
            )
          )
        )
        docked.copy(persisted =
          docked.persisted.copy(
            config = docked.persisted.config.withMarkdownViewMode(MarkdownViewMode.SplitPreview),
            focus = Focus.Surface(SurfaceId("outline"))
          )
        )
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "markdown-view-source",
          "Switch Markdown rendering back to source mode.",
          CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.Source)),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.config.markdownViewMode shouldBe MarkdownViewMode.Source
    state.pinnedSurfaces.map(_.id) should contain only SurfaceId("outline")
    state.persisted.focus shouldBe Focus.Surface(SurfaceId("outline"))
  }

  it should "paint split previews with the editor's text pipeline, restyled and without markers, not as an image" in {
    val state   = markdownPreviewPanelState("a **bold** b", CursorPosition(0, 0))
    val surface = new MockRenderSurface(120, 32)

    renderPanel(state, surface)

    surface.drawImageCalls shouldBe empty
    val panelRuns = panelRunsOf(state, surface)
    panelRuns.exists(call => call.s == "bold" && call.activeStyle.isBold) shouldBe true
    panelRuns.exists(_.s.contains("*")) shouldBe false
  }

  it should "start the preview's text flush with the docked panel's content" in {
    val state   = markdownPreviewPanelState("a **bold** b", CursorPosition(0, 0))
    val surface = new MockRenderSurface(120, 32)

    renderPanel(state, surface)

    val contentRect = previewContentRect(state)
    val charWidth   = CellMetrics.fromFont(previewFont).charWidth
    val firstRun    = panelRunsOf(state, surface).minBy(_.xPx)
    firstRun.xPx should be >= (contentRect.x * charWidth).toFloat
    firstRun.xPx should be < ((contentRect.x + contentRect.width) * charWidth).toFloat
  }

  it should "paint no image for a preview on a HiDPI surface either" in {
    val state   = markdownPreviewPanelState("a **bold** b", CursorPosition(0, 0))
    val surface = new HiDpiMockRenderSurface(120, 32, scaleX = 2.0, scaleY = 2.0)

    renderPanel(state, surface)

    surface.drawImageCalls shouldBe empty
    panelRunsOf(state, surface).exists(call => call.s == "bold" && call.activeStyle.isBold) shouldBe true
  }

  it should "show the document from the editor's top line, wherever the cursor is" in {
    val source = (1 to 200).map(i => s"Heading $i").mkString("\n")

    val fromTop  = shownInPanel(markdownPreviewPanelState(source, CursorPosition(0, 0)))
    val scrolled = shownInPanel(markdownPreviewPanelState(source, CursorPosition(180, 0), topLine = 170))

    fromTop.exists(_.contains("Heading 1")) shouldBe true
    fromTop.exists(_.contains("Heading 180")) shouldBe false
    scrolled.exists(_.contains("Heading 180")) shouldBe true
    scrolled.exists(_.contains("Heading 1 ")) shouldBe false
  }

  it should "show an edit at once, even mid-burst, with no stale picture to wait out" in {
    val bufferId = BufferId(1)
    val settled  = markdownPreviewPanelState("# First settled render", CursorPosition(0, 0))
    val midBurst = markdownPreviewPanelState("# Totally different content mid-burst", CursorPosition(0, 0))
    val bursting = midBurst.copy(persisted =
      midBurst.persisted.copy(buffers =
        midBurst.persisted.buffers.updatedWith(bufferId)(
          _.map(_.copy(markdownPreviewEditGeneration = 1L, markdownPreviewCommittedGeneration = 0L))
        )
      )
    )

    // The heading is set larger than the panel's text, so its words wrap onto rows of their own.
    shownInPanel(settled).mkString(" ") should (include("First") and include("settled") and include("render"))
    shownInPanel(bursting).mkString(" ") should (include("Totally") and include("different") and include("mid-burst"))
    shownInPanel(bursting).mkString(" ") should not include "settled"
  }

  "Markdown inline lens mode" should "leave markdown source untouched in source mode" in {
    val surface = new MockRenderSurface(100, 20)
    val font    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    RendererEntryPoints.render(
      markdownEditorState(MarkdownViewMode.Source),
      cursorVisible = true,
      surface,
      ViewportSize(100, 20),
      codeFont = FontSpec.fromAwt(font),
      textFont = FontSpec.fromAwt(font),
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None,
      com.serenity.state.manager.RenderCaches.create()
    )

    val rows = surfaceRows(surface)
    rows.exists(_.contains("# Rendered")) shouldBe true
    rows.exists(_.contains("# Raw")) shouldBe true
  }

  it should "render markdown preview as an image and overlay the active block as raw source" in {
    val surface = new MockRenderSurface(100, 20)
    val font    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    RendererEntryPoints.render(
      markdownEditorState(MarkdownViewMode.InlineLens),
      cursorVisible = true,
      surface,
      ViewportSize(100, 20),
      codeFont = FontSpec.fromAwt(font),
      textFont = FontSpec.fromAwt(font),
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None,
      com.serenity.state.manager.RenderCaches.create()
    )

    val rows = surfaceRows(surface)
    surface.drawImageCalls should have size 1
    rows.exists(_.contains("# Rendered")) shouldBe false
    rows.exists(_.contains("# Raw")) shouldBe true
    rows.exists(_.contains("continued")) shouldBe false
  }

  it should "render inline lens preview images at device scale on HiDPI surfaces" in {
    val font    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val surface = new HiDpiMockRenderSurface(100, 20, scaleX = 2.0, scaleY = 2.0)

    RendererEntryPoints.render(
      markdownEditorState(MarkdownViewMode.InlineLens),
      cursorVisible = true,
      surface,
      ViewportSize(100, 20),
      codeFont = FontSpec.fromAwt(font),
      textFont = FontSpec.fromAwt(font),
      cellMetrics = metrics,
      cursorColor = None,
      com.serenity.state.manager.RenderCaches.create()
    )

    surface.drawImageCalls should have size 1
    val drawn = surface.drawImageCalls.head
    drawn.image.getWidth shouldBe drawn.width * metrics.charWidth * 2
    drawn.image.getHeight shouldBe drawn.height * metrics.lineHeight * 2
  }

  it should "render inactive markdown tables through the markdown preview image in inline lens mode" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val state = AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(
        buffers = Map(
          bufferId -> {
            val base = Buffer.fromString(
              bufferId,
              """|| Task | Owner |
                || ---- | ----- |
                || Ship | Codex |
                |
                |Editing here""".stripMargin
            )
            base.copy(
              document = base.document.copy(language = Some(LanguageId.Markdown)),
              editing = EditingState(List(CursorPosition(4, 0))),
              viewport = Viewport.default.copy(visibleLines = 10)
            )
          }
        ),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppConfig.default
          .withSyntaxHighlighting(true)
          .withLineNumbers(false)
          .withoutStatusLine
          .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      )
    )
    val surface = new MockRenderSurface(100, 20)
    val font    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(100, 20),
      codeFont = FontSpec.fromAwt(font),
      textFont = FontSpec.fromAwt(font),
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None,
      com.serenity.state.manager.RenderCaches.create()
    )

    surface.drawImageCalls should have size 1
    surfaceRows(surface).exists(_.contains("| ---- | ----- |")) shouldBe false
  }

  private def surfaceRows(surface: MockRenderSurface): List[String] =
    (0 until surface.height).map(surface.getRow).map(_.trim).filter(_.nonEmpty).toList

  private class HiDpiMockRenderSurface(width: Int, height: Int, scaleX: Double, scaleY: Double)
      extends MockRenderSurface(width, height):
    override def devicePixelScaleX: Double = scaleX
    override def devicePixelScaleY: Double = scaleY

  private def markdownPreviewPanelState(source: String, cursor: CursorPosition, topLine: Int = 0): AppState =
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val baseState = AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(
        buffers = Map(
          bufferId -> {
            val base = Buffer.fromString(bufferId, source)
            base.copy(
              document = base.document.copy(language = Some(LanguageId.Markdown)),
              editing = EditingState(List(cursor)),
              viewport = Viewport.default.copy(visibleLines = 10, topLine = topLine)
            )
          }
        ),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppConfig.default
          .withLineNumbers(false)
          .withoutStatusLine
          .withMarkdownViewMode(MarkdownViewMode.SplitPreview)
      ),
      runtime = AppState.empty.runtime
    )
    DockedPanelFixtures.dock(
      baseState,
      SurfaceId("markdown-preview"),
      SurfaceContent.MarkdownPreview(bufferId, "notes.md"),
      PanelPosition.Right,
      40
    )

  private val previewFont     = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val previewViewport = ViewportSize(120, 32)

  private def renderPanel(state: AppState, surface: MockRenderSurface): Unit =
    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      previewViewport,
      codeFont = FontSpec.fromAwt(previewFont),
      textFont = FontSpec.fromAwt(previewFont),
      cellMetrics = CellMetrics.fromFont(previewFont),
      cursorColor = None,
      com.serenity.state.manager.RenderCaches.create()
    )

  private def previewContentRect(state: AppState): LayoutRect =
    EditorLayoutContract
      .from(state, previewViewport, LayoutEngine.calculateLayout(state, previewViewport))
      .pinnedSurfaceContentRects(SurfaceId("markdown-preview"))

  /** The glyph runs drawn inside the docked preview panel, not the editor pane beside it. */
  private def panelRunsOf(state: AppState, surface: MockRenderSurface): List[surface.DrawRunPxCall] =
    val panelLeftPx = (previewContentRect(state).x * CellMetrics.fromFont(previewFont).charWidth).toFloat
    surface.drawRunPxCalls.filter(_.xPx >= panelLeftPx)

  private def shownInPanel(state: AppState): List[String] =
    val surface = new MockRenderSurface(120, 32)
    renderPanel(state, surface)
    panelRunsOf(state, surface).map(_.s)

end MarkdownViewModeSpec
