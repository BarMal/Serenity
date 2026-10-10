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

/** What the editor paints for Markdown emphasis and code in live preview and read mode: the styled text without its
  * markers, and the markers themselves on the lines the caret is on.
  */
class MarkdownInlineRenderingSpec extends AnyFlatSpec with Matchers:

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

  "Read mode" should "paint strong text bold and draw none of its markers" in {
    val runs = paint(editorState("a **bold** b", MarkdownViewMode.Read, CursorPosition(0, 0))).drawRunPxCalls

    runs.exists(call => call.s == "bold" && call.activeStyle.isBold) shouldBe true
    runs.exists(_.s.contains("*")) shouldBe false
  }

  it should "paint emphasised text italic" in {
    val runs = paint(editorState("an *italic* word", MarkdownViewMode.Read, CursorPosition(0, 0))).drawRunPxCalls

    runs.exists(call => call.s == "italic" && call.activeStyle.isItalic && !call.activeStyle.isBold) shouldBe true
    runs.exists(_.s.contains("*")) shouldBe false
  }

  it should "paint code spans on the raised surface colour" in {
    val state = editorState("a `code` b", MarkdownViewMode.Read, CursorPosition(0, 0))
    val runs  = paint(state).drawRunPxCalls

    runs.exists(call => call.s == "code" && call.background == state.persisted.theme.panel.background) shouldBe true
    runs.exists(_.s.contains("`")) shouldBe false
  }

  it should "hide the markers on the line the caret is on too" in {
    val runs = paint(editorState("a **bold** b", MarkdownViewMode.Read, CursorPosition(0, 6))).drawRunPxCalls

    runs.exists(_.s.contains("*")) shouldBe false
  }

  "Live preview" should "show the markers on the caret line and hide them on every other line" in {
    val runs =
      paint(editorState("**a** x\n**b** y", MarkdownViewMode.LivePreview, CursorPosition(0, 0))).drawRunPxCalls

    runs.count(_.s == "**") shouldBe 2
    runs.exists(call => call.s == "a" && call.activeStyle.isBold) shouldBe true
    runs.exists(call => call.s == "b" && call.activeStyle.isBold) shouldBe true
  }

  it should "move the revealed markers with the caret" in {
    val runs =
      paint(editorState("**a** x\n**b** y", MarkdownViewMode.LivePreview, CursorPosition(1, 0))).drawRunPxCalls

    val markers = runs.filter(_.s == "**")
    markers should have size 2
    markers.map(_.yPx).distinct should have size 1
    markers.head.yPx should be > runs.filter(_.s == "a").head.yPx
  }

  it should "leave a fenced code block as written" in {
    val runs = paint(
      editorState("text\n```\n**not bold**\n```\ntext", MarkdownViewMode.LivePreview, CursorPosition(0, 0))
    ).drawRunPxCalls

    runs.exists(_.s.contains("**not bold**")) shouldBe true
  }

  "Source mode" should "paint markdown markers like any other text" in {
    val runs = paint(editorState("a **bold** b", MarkdownViewMode.Source, CursorPosition(0, 0))).drawRunPxCalls

    runs.exists(_.s.contains("**bold**")) shouldBe true
  }
