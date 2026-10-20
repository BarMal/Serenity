package com.serenity

import java.awt.Font

import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the editor paints for Markdown headings, lists, quotes, rules and tables in live preview and read mode. */
class MarkdownBlockRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val codeFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont = Font(Font.SANS_SERIF, Font.PLAIN, 14)

  private def editorState(source: String, mode: MarkdownViewMode, cursor: CursorPosition): AppState =
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val base     = Buffer.fromString(bufferId, source)
    val buffer = base.copy(
      document = base.document.copy(language = Some(LanguageId.Markdown)),
      editing = EditingState(List(cursor)),
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
        config = AppConfig.default.withLineNumbers(false).withoutStatusLine.withMarkdownViewMode(mode)
      )
    )

  private def paint(state: AppState): MockRenderSurface =
    val surface = new MockRenderSurface(100, 20)
    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(100, 20),
      codeFont = FontSpec.fromAwt(codeFont),
      textFont = FontSpec.fromAwt(textFont),
      cellMetrics = CellMetrics.fromFont(codeFont),
      cursorColor = None,
      RenderCaches.create()
    )
    surface

  private def thinMutedRules(state: AppState, surface: MockRenderSurface) =
    surface.fillPixelRectCalls.filter(call =>
      call.color == state.persisted.theme.muted && call.heightPx <= 2 && call.widthPx > 20
    )

  "Read mode" should "paint a heading larger and bold, without its hashes" in {
    val runs = paint(editorState("## Title\nbody", MarkdownViewMode.Read, CursorPosition(1, 0))).drawRunPxCalls

    runs.exists(call =>
      call.s == "Title" && call.activeStyle.isBold && call.activeStyle.fontSize.exists(_ > 14.0f)
    ) shouldBe true
    runs.exists(_.s.contains("#")) shouldBe false
  }

  it should "give a heading's row more height than a body row" in {
    val runs  = paint(editorState("# Title\nbody", MarkdownViewMode.Read, CursorPosition(1, 0))).drawRunPxCalls
    val title = runs.find(_.s == "Title")
    val body  = runs.find(_.s == "body")

    title.map(_.lineHeightPx).getOrElse(0) should be > body.map(_.lineHeightPx).getOrElse(Int.MaxValue)
  }

  it should "paint a list bullet muted and bold, and keep the item text" in {
    val state = editorState("- item", MarkdownViewMode.Read, CursorPosition(0, 0))
    val runs  = paint(state).drawRunPxCalls

    runs.exists(call =>
      call.s == "-" && call.activeStyle.isBold && call.foreground == state.persisted.theme.muted
    ) shouldBe true
    runs.exists(_.s.contains("item")) shouldBe true
  }

  it should "paint a block quote's text in italics" in {
    val runs = paint(editorState("> wise words", MarkdownViewMode.Read, CursorPosition(0, 0))).drawRunPxCalls

    runs.exists(call => call.s.contains("wise words") && call.activeStyle.isItalic) shouldBe true
  }

  it should "draw a thematic break as a line instead of its characters" in {
    val state   = editorState("above\n---\nbelow", MarkdownViewMode.Read, CursorPosition(0, 0))
    val surface = paint(state)

    surface.drawRunPxCalls.exists(_.s.contains("--")) shouldBe false
    thinMutedRules(state, surface) should have size 1
  }

  it should "draw a table's delimiter row as a line and dim its pipes" in {
    val state   = editorState("| a | b |\n|---|---|\n| 1 | 2 |", MarkdownViewMode.Read, CursorPosition(0, 0))
    val surface = paint(state)

    surface.drawRunPxCalls.exists(_.s.contains("---")) shouldBe false
    thinMutedRules(state, surface) should have size 1
    surface.drawRunPxCalls.exists(call => call.s == "|" && call.foreground == state.persisted.theme.muted) shouldBe true
  }

  "Live preview" should "show a heading's hashes only on the caret line" in {
    val onHeading =
      paint(editorState("# One\n# Two", MarkdownViewMode.LivePreview, CursorPosition(0, 0))).drawRunPxCalls

    onHeading.count(_.s.contains("#")) shouldBe 1
    onHeading.exists(call => call.s.contains("Two") && call.activeStyle.isBold) shouldBe true
  }

  it should "show a thematic break's characters, and no drawn line, on the caret line" in {
    val state   = editorState("above\n---\nbelow", MarkdownViewMode.LivePreview, CursorPosition(1, 1))
    val surface = paint(state)

    surface.drawRunPxCalls.exists(_.s.contains("---")) shouldBe true
    thinMutedRules(state, surface) shouldBe empty
  }

  "Read mode with block syntax" should "paint struck text with a strike-through and none of its tildes" in {
    val runs = paint(editorState("a ~~gone~~ b", MarkdownViewMode.Read, CursorPosition(0, 0))).drawRunPxCalls

    runs.exists(call => call.s == "gone" && call.activeStyle.isStrikethrough) shouldBe true
    runs.exists(_.s.contains("~")) shouldBe false
  }

  it should "paint a setext heading like a hash heading and its underline as a line" in {
    val state   = editorState("Title\n=====\nbody", MarkdownViewMode.Read, CursorPosition(2, 0))
    val surface = paint(state)

    surface.drawRunPxCalls.exists(call =>
      call.s == "Title" && call.activeStyle.isBold && call.activeStyle.fontSize.exists(_ > 14.0f)
    ) shouldBe true
    surface.drawRunPxCalls.exists(_.s.contains("=")) shouldBe false
    thinMutedRules(state, surface) should have size 1
  }

  it should "line a table's pipes up from row to row" in {
    val state = editorState("| a | b |\n|---|---|\n| ccccc | d |", MarkdownViewMode.Read, CursorPosition(0, 0))
    val pipes =
      paint(state).drawRunPxCalls.filter(_.s == "|").groupBy(_.yPx).toList.sortBy(_._1).map(_._2.map(_.xPx).sorted)

    pipes should have size 2
    pipes.map(_.size) shouldBe List(3, 3)
    pipes.flatMap(_.headOption).distinct should have size 1
    pipes.lift(0).zip(pipes.lift(1)).foreach {
      case (header, row) => header.zip(row).foreach { case (a, b) => a shouldBe (b +- 0.1f) }
    }
  }

  "Source mode" should "paint headings, lists and rules as written" in {
    val state   = editorState("# Title\n- item\n---", MarkdownViewMode.Source, CursorPosition(0, 0))
    val surface = paint(state)

    surface.drawRunPxCalls.map(_.s).mkString should include("# Title")
    thinMutedRules(state, surface) shouldBe empty
  }
