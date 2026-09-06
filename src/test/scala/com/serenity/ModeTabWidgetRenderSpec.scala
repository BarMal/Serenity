package com.serenity

import com.serenity.config.{AppMode, CornerPosition}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The mode indicator (issue #1307): an always-visible glyph for the current `AppMode`, at the corner
  * `AppConfig.modeTabWidgetCornerPosition` selects, alongside wherever the current tab's name is already shown.
  *
  * It is folded into whichever chrome text already owns that corner -- the gutter row for `BottomLeft`/ `BottomRight`,
  * the active pane's header for `TopLeft`/`TopRight` -- rather than painted as an independent overlay, and it carries
  * only the glyph, not the tab name itself: the chrome text it joins already shows that (the gutter's own filename
  * segment, or the header's title verbatim), so repeating it would just show the same name twice. An earlier version
  * drew a standalone rect positioned from raw viewport coordinates; it silently clobbered unrelated content in several
  * other renderer specs (a pane header sharing row 0, markdown-lens/command-palette content sharing whatever row the
  * default bottom-right corner landed on) because nothing reserves screen space for it. Sharing the gutter/header's own
  * already-reserved row -- the same trick `wordCountStatusText` already uses on the gutter -- guarantees it never
  * overwrites anything else.
  */
class ModeTabWidgetRenderSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def stateWithBuffer(
    title: String,
    mode: AppMode = AppMode.Code,
    corner: CornerPosition = CornerPosition.BottomRight
  ): AppState =
    val buffer0 = Buffer.fromString(BufferId(0), "content")
    val buffer  = buffer0.copy(document = buffer0.document.copy(filePath = Some(java.nio.file.Paths.get(title))))
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0)),
        config = AppState.initial.persisted.config.withAppMode(mode).withModeTabWidgetCornerPosition(corner)
      )
    )

  private def renderedText(state: AppState): String =
    val surface  = new MockRenderSurface(100, 24)
    val viewport = ViewportSize(100, 24)
    Renderer.render(state, cursorVisible = true, surface, viewport)
    surface.drawRunPxCalls.map(_.s).mkString("\n")

  "The mode indicator" should "append the glyph to the gutter in the bottom-right corner by default" in {
    renderedText(stateWithBuffer("notes.md")).linesIterator.find(_.contains("Line")) shouldBe
      Some(" Line 1, Col 1 | Language: Plain Text | notes.md [C] ")
  }

  it should "prepend the glyph to the gutter for the bottom-left corner" in {
    renderedText(stateWithBuffer("notes.md", corner = CornerPosition.BottomLeft)).linesIterator
      .find(_.contains("Line")) shouldBe Some(" [C] Line 1, Col 1 | Language: Plain Text | notes.md ")
  }

  it should "use the prose glyph in prose mode" in {
    renderedText(stateWithBuffer("notes.md", mode = AppMode.Prose)) should include("[P]")
  }

  it should "fold into the active pane's header for the top-left corner, before the tab title" in {
    renderedText(stateWithBuffer("notes.md", corner = CornerPosition.TopLeft)) should include("[C] notes.md")
  }

  it should "fold into the active pane's header for the top-right corner, after the tab title" in {
    renderedText(stateWithBuffer("notes.md", corner = CornerPosition.TopRight)) should include("notes.md [C]")
  }

  it should "leave the gutter's own text untouched for a top corner" in {
    renderedText(stateWithBuffer("notes.md", corner = CornerPosition.TopLeft)).linesIterator
      .find(_.contains("Line")) shouldBe Some(" Line 1, Col 1 | Language: Plain Text | notes.md ")
  }

  it should "leave the header's own title untouched for a bottom corner" in {
    renderedText(stateWithBuffer("notes.md", corner = CornerPosition.BottomRight)).linesIterator
      .find(_.trim == "notes.md") shouldBe Some("notes.md")
  }

  it should "still show the mode glyph in the gutter when no tab is focused" in {
    renderedText(AppState.empty) should include("[C]")
  }
