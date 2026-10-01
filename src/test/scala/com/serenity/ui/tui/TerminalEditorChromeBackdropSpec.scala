package com.serenity.ui.tui

import java.awt.{Color, Font}
import java.io.StringWriter

import com.serenity.TestWorkspaceTrees
import com.serenity.config.{AppConfig, MarkdownViewMode, StatusLineColors, StatusLinePlacement}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{
  AppState,
  Buffer,
  BufferId,
  CursorPosition,
  EditingState,
  EditorPane,
  Focus,
  PaneId,
  Viewport
}
import com.serenity.ui.layout.{
  CellMetrics,
  Layout,
  SplitAxis,
  ViewportSize,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.{DefaultThemes, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The editor's own chrome -- line-number gutter, pinned status gutter, inactive pane header, raw-source markdown lens
  * -- paints the theme's panel colour as material standing in for the background. Under a theme whose background is the
  * terminal's own, the TUI shows that backdrop there too, while the gutter divider and the active pane's highlighted
  * header keep their deliberate contrast.
  */
class TerminalEditorChromeBackdropSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font        = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val transparent = DefaultThemes.transparent
  private val viewport    = ViewportSize(60, 16)
  private val paneId      = PaneId(0)
  private val bufferId    = BufferId(1)

  private def sameRgb(a: Color, b: Color): Boolean = (a.getRGB & 0xffffff) == (b.getRGB & 0xffffff)
  private def isBackdrop(color: Color): Boolean    = color == TerminalEmulator.TransparentBackground

  private def render(state: AppState): TerminalEmulator =
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(viewport.width, viewport.height, writer, CellMetrics.cellUnit)
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      font,
      font,
      CellMetrics.cellUnit,
      None,
      RenderCaches.create()
    )
    surface.flush()
    TerminalEmulator.blank(viewport.width, viewport.height).consume(writer.toString)

  private def editorState(
    theme: Theme,
    config: AppConfig = AppConfig.default,
    text: String = "alpha\nbeta\ngamma",
    configureBuffer: Buffer => Buffer = identity
  ): AppState =
    val initial = AppState.initial
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> configureBuffer(Buffer.fromString(bufferId, text))),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId),
        theme = theme,
        config = config
      )
    )

  private def lineNumberCell(screen: TerminalEmulator): (Int, Int) =
    val (alphaCol, row) = screen.find("alpha").getOrElse(fail(screen.render))
    val digitCol        = screen.rowText(row).take(alphaCol).lastIndexOf('1')
    withClue(s"no line number before 'alpha':\n${screen.render}\n")(digitCol should be >= 0)
    (digitCol, row)

  "the line-number gutter in the TUI under a transparent theme" should
    "sit on the terminal's own background, keeping its divider" in {
      val screen = render(editorState(transparent))

      val (digitCol, row) = lineNumberCell(screen)
      val (alphaCol, _)   = screen.find("alpha").getOrElse(fail(screen.render))
      withClue(screen.render)(isBackdrop(screen.cellAt(digitCol, row).bg) shouldBe true)
      val gutterBackgrounds = (0 until alphaCol).map(col => screen.cellAt(col, row).bg)
      withClue(s"$gutterBackgrounds\n${screen.render}\n")(
        gutterBackgrounds.exists(sameRgb(_, transparent.panelBorder)) shouldBe true
      )
      gutterBackgrounds.filterNot(sameRgb(_, transparent.panelBorder)).foreach(bg => isBackdrop(bg) shouldBe true)
    }

  "the line-number gutter in the TUI under an opaque theme" should "keep the theme's panel colour" in {
    val theme  = Theme.dark
    val screen = render(editorState(theme))

    val (digitCol, row) = lineNumberCell(screen)
    sameRgb(screen.cellAt(digitCol, row).bg, theme.panel.background) shouldBe true
  }

  "the pinned status gutter in the TUI under a transparent theme" should "sit on the terminal's own background" in {
    val config = AppConfig.default.withStatusLinePlacement(StatusLinePlacement.Pinned)
    val screen = render(editorState(transparent, config))

    val lastRow = viewport.height - 1
    withClue(screen.render)(screen.rowText(lastRow).trim should not be empty)
    (0 until viewport.width).foreach { col =>
      withClue(s"cell ($col, $lastRow):\n${screen.render}\n")(isBackdrop(screen.cellAt(col, lastRow).bg) shouldBe true)
    }
  }

  it should "keep a background it was explicitly configured with" in {
    val config = AppConfig.default
      .withStatusLinePlacement(StatusLinePlacement.Pinned)
      .withStatusLineColors(StatusLineColors(background = Some(Color.BLUE)))
    val screen = render(editorState(transparent, config))

    sameRgb(screen.cellAt(viewport.width - 1, viewport.height - 1).bg, Color.BLUE) shouldBe true
  }

  "pane headers in the TUI under a transparent theme" should
    "show the backdrop behind an inactive header while the active one keeps its highlight" in {
      val secondPane   = PaneId(1)
      val secondBuffer = BufferId(2)
      val single       = editorState(transparent)
      val layout       = single.persisted.layout
      val state = single.copy(persisted =
        single.persisted.copy(
          buffers = single.persisted.buffers + (secondBuffer -> Buffer.fromString(secondBuffer, "delta")),
          bufferOrder = List(bufferId, secondBuffer),
          layout = layout.copy(
            editorPanes = layout.editorPanes + (secondPane -> EditorPane.withBuffer(secondPane, secondBuffer)),
            // Stacked, so the inactive pane's header has a row of its own: side by side, the active header spans the
            // workspace and covers it.
            workspaceTree = Some(
              WorkspaceTree(
                WorkspaceNode.Split(
                  WorkspaceNodeId("split"),
                  SplitAxis.Vertical,
                  0.5,
                  WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId),
                  WorkspaceNode.Leaf(WorkspaceNodeId("editor-1"), secondPane)
                )
              )
            )
          )
        )
      )
      val screen = render(state)

      // Row 0 is the tab strip, which names both buffers too.
      def headerCell(title: String): (Int, Int) =
        screen.rows.zipWithIndex
          .collectFirst { case (line, row) if row > 0 && line.contains(title) => (line.indexOf(title), row) }
          .getOrElse(fail(s"no '$title' header:\n${screen.render}"))
      val (activeCol, activeRow)     = headerCell("Buffer 1")
      val (inactiveCol, inactiveRow) = headerCell("Buffer 2")
      withClue(screen.render)(
        sameRgb(screen.cellAt(activeCol, activeRow).bg, transparent.highlighted.background) shouldBe true
      )
      withClue(screen.render)(isBackdrop(screen.cellAt(inactiveCol, inactiveRow).bg) shouldBe true)
      // The line-number divider runs through the header row and keeps its own colour.
      (0 until viewport.width)
        .filterNot(col => sameRgb(screen.cellAt(col, inactiveRow).bg, transparent.panelBorder))
        .foreach { col =>
          withClue(s"cell ($col, $inactiveRow) ${screen.cellAt(col, inactiveRow).bg}:\n${screen.render}\n")(
            isBackdrop(screen.cellAt(col, inactiveRow).bg) shouldBe true
          )
        }
    }

  "the raw-source markdown lens in the TUI under a transparent theme" should "sit on the terminal's own background" in {
    val config = AppConfig.default.withLineNumbers(false).withMarkdownViewMode(MarkdownViewMode.InlineLens)
    val state = editorState(
      transparent,
      config,
      text = "# Lens\n\n# Raw\ncontinued",
      configureBuffer = buffer =>
        buffer.copy(
          document = buffer.document.copy(language = Some(LanguageId.Markdown)),
          editing = EditingState(List(CursorPosition(2, 0))),
          viewport = Viewport.default.copy(visibleLines = 10)
        )
    )
    val screen = render(state)

    val (rawCol, rawRow) = screen.find("# Raw").getOrElse(fail(s"no raw lens:\n${screen.render}"))
    (rawCol until viewport.width - 1).foreach { col =>
      withClue(s"cell ($col, $rawRow):\n${screen.render}\n")(isBackdrop(screen.cellAt(col, rawRow).bg) shouldBe true)
    }
  }
end TerminalEditorChromeBackdropSpec
