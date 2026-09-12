package com.serenity

import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun, RichTextStyle}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1482: regular (unselected) prose rendered at a different font size than the same text once selected. The two
  * paint passes shared one `RenderSurface` and one active buffer font, but only the normal glyph pass
  * ([[com.serenity.ui.renderer.CharacterRenderer.renderMeasuredLineWithAnimation]]) ever applied a run's own
  * [[com.serenity.ui.theme.TextStyle]] (font family/size/weight) via `enableStyle`/`disableStyle` before painting --
  * the selection/comment/diagnostic highlight overlays in
  * [[com.serenity.ui.renderer.RendererHighlights]] repainted the same glyphs with whatever style the surface
  * happened to be left at, silently dropping the run's own style. For a document with an explicit per-run font size
  * (a rich-text prose paragraph, or an imported document with its own point size), that made the highlighted copy
  * render at the *surface's* base size while the underlying, unselected glyphs kept their own -- exactly the
  * mismatch reported.
  */
class SelectionFontStyleConsistencySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "RendererEntryPoints.render" should "resolve the same font size for selected prose as for the same text unselected" in {
    val paneId    = PaneId(0)
    val bufferId  = BufferId(1)
    val proseText = "Hello World"
    val richDoc = RichTextDocument(
      List(
        RichTextParagraph(
          List(RichTextRun(proseText, RichTextStyle(fontSize = Some(30.0f)))),
          ParagraphAlignment.Left,
          ParagraphRole.Body
        )
      )
    )
    val buffer = Buffer
      .fromString(bufferId, proseText)
      .copy(
        richText = RichTextState(richTextDocument = Some(richDoc)),
        editing = EditingState(
          cursors = List(CursorPosition(0, 6)),
          selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 11)))
        )
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
      )
    )

    val surface = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val unselectedDraw =
      surface.drawRunPxCalls.find(_.s == proseText).getOrElse(fail("Expected the unselected prose run to be drawn"))
    val selectedDraw =
      surface.drawRunPxCalls.find(_.s == "World").getOrElse(fail("Expected the selection highlight to redraw 'World'"))

    selectedDraw.activeStyle.fontSize shouldBe unselectedDraw.activeStyle.fontSize
  }

