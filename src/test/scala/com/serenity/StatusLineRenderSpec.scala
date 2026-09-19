package com.serenity

import com.serenity.testkit.EditingStateFixtures

import com.serenity.config.{AppMode, StatusLinePlacement, StatusSegment}
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, ViewportSize}
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the status line actually paints, asserted against `MockRenderSurface.drawRunPxCalls` -- the mode is a word, not
  * an unexplained glyph (#1534); word counts follow the selection; off paints nothing.
  */
class StatusLineRenderSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def stateWith(
    text: String,
    configure: com.serenity.config.AppConfig => com.serenity.config.AppConfig = identity,
    language: Option[LanguageId] = None,
    title: Option[String] = None,
    selection: Option[Selection] = None
  ): AppState =
    val buffer0 = Buffer.fromString(BufferId(1), text)
    val buffer = buffer0.copy(
      document = buffer0.document.copy(language = language, filePath = title.map(java.nio.file.Paths.get(_))),
      editing = EditingStateFixtures(selection = selection)
    )
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
        ),
        focus = Focus.EditorPane(PaneId(0)),
        theme = Theme.light,
        config = configure(AppState.initial.persisted.config)
      )
    )

  private def rendered(state: AppState): MockRenderSurface =
    val surface = new MockRenderSurface(100, 24)
    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 24))
    surface

  /** The pinned row is a gutter, painted as measured pixel runs. */
  private def painted(state: AppState): String =
    rendered(state).drawRunPxCalls.map(_.s).mkString("\n")

  /** The floating row is an overlay, painted cell by cell like every other floating surface. */
  private def paintedCells(state: AppState): String =
    val surface = rendered(state)
    (0 until 24).map(y => (0 until 100).map(x => surface.getChar(x, y)).mkString).mkString("\n")

  "the status line" should "paint position, language, title and mode as words by default" in {
    painted(stateWith("content", title = Some("notes.md"), language = Some(LanguageId.Markdown))) should include(
      "Line 1, Col 1 | Markdown | notes.md | Code"
    )
  }

  it should "name the prose workspace as Prose and an unsaved plain buffer as such" in {
    painted(stateWith("content", _.withAppMode(AppMode.Prose))) should include("Plain Text | Unsaved | Prose")
  }

  it should "paint the whole-buffer word and character count when those segments are shown" in {
    val counts = List(StatusSegment.WordCount, StatusSegment.CharCount, StatusSegment.ReadingTime)
    val text   = painted(stateWith("hello brave new world", _.withStatusLineSegments(counts)))

    text should include("4 words")
    text should include("21 chars")
    text should include("min read")
  }

  it should "scope the word count to a non-empty selection" in {
    val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
    val state = stateWith(
      "hello brave new world",
      _.withStatusLineSegments(List(StatusSegment.WordCount)),
      selection = Some(selection)
    )

    painted(state) should include("1 of 4 words selected")
  }

  it should "fall back to the whole-buffer count when the selection is empty" in {
    val selection = Selection(CursorPosition(0, 3), CursorPosition(0, 3))
    val state = stateWith(
      "hello brave new world",
      _.withStatusLineSegments(List(StatusSegment.WordCount)),
      selection = Some(selection)
    )

    painted(state) should include("4 words")
    painted(state) should not include "selected"
  }

  it should "paint nothing of itself when off" in {
    val text = painted(stateWith("content", _.withoutStatusLine))

    text should not include "Line 1, Col 1"
    text should not include "Code"
  }

  it should "paint the floating placement as an overlay row that follows the caret" in {
    val state = stateWith("content", _.withStatusLinePlacement(StatusLinePlacement.Floating))

    paintedCells(state) should include("Line 1, Col 1 | Plain Text | Unsaved | Code")
    painted(state) should not include "Line 1, Col 1"
  }
