package com.serenity

import java.awt.Font

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints, RendererHighlights}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A term with a note is washed with the accent wherever it occurs, but only while the notes pane is open on the
  * document -- the pane is opened on purpose, so the highlights belong to looking at notes.
  */
class KeywordHighlightRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val propFont     = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val monoMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(80, 24)

  private val manuscriptId = BufferId(1)
  private val noteId       = BufferId(2)

  private val text = "Liz saw Lizard and Liz again."

  private def stateWith(notesPaneOpen: Boolean = true, visible: Boolean = true): AppState =
    val manuscriptPane = PaneId(0)
    val notesPane      = PaneId(1)
    val base           = Buffer.fromString(manuscriptId, text)
    val manuscript = base.copy(
      document = base.document.copy(language = Some(LanguageId.Markdown)),
      annotations = Annotations(notes = Map(NoteKey.Keyword("liz") -> Notes(noteId)))
    )
    val note    = Buffer.fromString(noteId, "Liz is the captain").copy(hidden = true)
    val initial = AppState.initial
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(manuscriptId -> manuscript, noteId -> note),
        bufferOrder = List(manuscriptId),
        layout = Layout(
          editorPanes = Map(
            manuscriptPane -> EditorPane.withBuffer(manuscriptPane, manuscriptId),
            notesPane      -> EditorPane.withBuffer(notesPane, noteId)
          ),
          activeEditorPaneId = Some(manuscriptPane),
          workspaceTree = Some(TestWorkspaceTrees.linear(manuscriptPane, notesPane))
        ),
        theme = Theme.light
      ),
      runtime = initial.runtime.copy(
        notesPane = Option.when(notesPaneOpen)(NotesPane(notesPane, manuscriptPane)),
        keywordHighlightsVisible = visible
      )
    )

  private def measuredSurface() = new MockRenderSurface(viewportSize.width, viewportSize.height)

  private def render(state: AppState): MockRenderSurface =
    val surface = measuredSurface()
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewportSize,
      FontSpec.fromAwt(monoFont),
      FontSpec.fromAwt(propFont),
      monoMetrics,
      None,
      RenderCaches.create()
    )
    surface

  private def washed(surface: MockRenderSurface): List[String] =
    surface.drawRunPxCalls
      .filter(_.background == RendererHighlights.keywordBackground(Theme.light))
      .map(_.s)

  "A term with a note" should "be washed wherever it occurs as a whole word while the notes pane is open" in {
    washed(render(stateWith())) shouldBe List("Liz", "Liz")
  }

  it should "not be washed when no notes pane is open" in {
    washed(render(stateWith(notesPaneOpen = false))) shouldBe Nil
  }

  it should "not be washed once the highlights are switched off" in {
    washed(render(stateWith(visible = false))) shouldBe Nil
  }

  it should "leave the document untouched" in {
    val state = stateWith()
    render(state)

    state.persisted.buffers(manuscriptId).document.content.collect() shouldBe text
  }
