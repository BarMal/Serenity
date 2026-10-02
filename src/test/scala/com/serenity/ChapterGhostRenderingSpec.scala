package com.serenity

import java.awt.Font

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The ghost is painted, faded, on the blank lines under an empty chapter -- and only there: it never changes the
  * document, and it goes the moment the chapter has prose or the ghosts are hidden.
  */
class ChapterGhostRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val propFont     = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val monoMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(80, 24)

  private val manuscriptId = BufferId(1)
  private val noteId       = BufferId(2)
  private val storm        = NoteKey.Chapter(HeadingIdentity("storm", 0))

  private val emptyChapter = "# Chapter 1: Storm\n\n\n# Chapter 2: Calm\nthe shore"
  private val proseChapter = "# Chapter 1: Storm\n\nThe sea rose.\n\n# Chapter 2: Calm\nthe shore"

  private def stateWith(text: String, ghostsVisible: Boolean = true): AppState =
    val paneId = PaneId(0)
    val base   = Buffer.fromString(manuscriptId, text)
    val manuscript = base.copy(
      document = base.document.copy(language = Some(LanguageId.Markdown)),
      annotations = Annotations(notes = Map(storm -> Notes(noteId)))
    )
    val note    = Buffer.fromString(noteId, "Gale hits\nLiz leaves").copy(hidden = true)
    val initial = AppState.initial
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(manuscriptId -> manuscript, noteId -> note),
        bufferOrder = List(manuscriptId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, manuscriptId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light
      ),
      runtime = initial.runtime.copy(chapterGhostsVisible = ghostsVisible)
    )

  private def render(state: AppState, surface: MockRenderSurface): MockRenderSurface =
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewportSize,
      monoFont,
      propFont,
      monoMetrics,
      None,
      RenderCaches.create()
    )
    surface

  private def cellSurface() =
    new MockRenderSurface(viewportSize.width, viewportSize.height, fontRenderContextOverride = None)

  private def measuredSurface() = new MockRenderSurface(viewportSize.width, viewportSize.height)

  "A chapter's ghost" should "be painted on the blank lines under an empty chapter" in {
    val surface    = render(stateWith(emptyChapter), cellSurface())
    val headingRow = surface.putStringCalls.find(_.s.contains("Storm")).map(_.y).getOrElse(fail("no heading painted"))

    surface.putStringCalls.filter(_.s.contains("Gale")).map(_.y).distinct shouldBe List(headingRow + 1)
    surface.putStringCalls.filter(_.s.contains("Liz")).map(_.y).distinct shouldBe List(headingRow + 2)
  }

  it should "be painted in the placeholder colour when the surface measures text" in {
    val surface = render(stateWith(emptyChapter), measuredSurface())

    val ghost = surface.drawRunPxCalls.filter(_.s == "Gale hits")
    ghost should have size 1
    ghost.map(_.foreground) shouldBe List(Theme.light.placeholder)
  }

  it should "not be painted once the chapter has prose" in {
    val surface = render(stateWith(proseChapter), cellSurface())

    surface.putStringCalls.exists(_.s.contains("Gale")) shouldBe false
    surface.putStringCalls.exists(_.s.contains("sea rose")) shouldBe true
  }

  it should "not be painted while ghosts are hidden" in {
    val surface = render(stateWith(emptyChapter, ghostsVisible = false), cellSurface())

    surface.putStringCalls.exists(_.s.contains("Gale")) shouldBe false
    surface.putStringCalls.exists(_.s.contains("Storm")) shouldBe true
  }

  it should "leave the document untouched" in {
    val state = stateWith(emptyChapter)
    render(state, cellSurface())

    state.persisted.buffers(manuscriptId).document.content.collect() shouldBe emptyChapter
  }
