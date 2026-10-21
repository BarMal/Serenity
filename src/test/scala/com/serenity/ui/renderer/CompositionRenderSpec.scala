package com.serenity.ui.renderer

import com.serenity.MockRenderSurface
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.ui.layout.{Layout, ViewportSize}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The real renderer, given the displayed state of an input method composing (#2008): the pre-edit reads inline in the
  * line, the text after it moved right, the clause being converted on the selection background.
  */
class CompositionRenderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val revisions = new CompositionRevisions

  private def shown(state: AppState): AppState = CompositionPreview.displayed(state, revisions)

  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(60, 12)

  private def stateWith(composition: Option[Composition]): AppState =
    val buffer = Buffer
      .fromString(bufferId, "hello world")
      .copy(
        editing = EditingState(List(CursorPosition(0, 2))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 10, visibleColumns = 50)
      )
    val base = AppState.initial(com.serenity.config.AppConfig.conventional)
    val state = base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(com.serenity.TestWorkspaceTrees.linear(PaneId(0)))
        ),
        theme = Theme.light
      )
    )
    state.copy(runtime = state.runtime.copy(composition = composition))

  private def composing(text: String, clauses: List[CompositionClause]): Composition =
    Composition(PaneId(0), bufferId, CursorPosition(0, 2), 0L, text, clauses, None)

  private def rendered(state: AppState): MockRenderSurface =
    val surface = new MockRenderSurface(viewport.width, viewport.height)
    RendererEntryPoints.render(state, cursorVisible = false, surface, viewport, RenderCaches.create())
    surface

  private def textRows(surface: MockRenderSurface): List[String] =
    (0 until viewport.height).map(surface.getRow).toList

  "The renderer" should "show the text unchanged when nothing is composing" in {
    textRows(rendered(shown(stateWith(None)))).exists(_.contains("hello world")) shouldBe true
  }

  it should "show the pre-edit inline, with the text after it moved right" in {
    val state = shown(
      stateWith(Some(composing("XY", List(CompositionClause(0, 2, selected = true)))))
    )

    textRows(rendered(state)).exists(_.contains("heXYllo world")) shouldBe true
  }

  it should "put the clause being converted on the selection background" in {
    val state = shown(
      stateWith(
        Some(composing("XY", List(CompositionClause(0, 1, selected = false), CompositionClause(1, 2, selected = true))))
      )
    )
    val surface = rendered(state)
    val row     = textRows(surface).indexWhere(_.contains("heXYllo"))
    val column  = textRows(surface)(row).indexOf("heXYllo")

    surface.getBg(column + 3, row) shouldBe Theme.light.selection.background
    surface.getBg(column + 2, row) should not be Theme.light.selection.background
    surface.getBg(column + 4, row) should not be Theme.light.selection.background
  }

  it should "underline the pre-edit and nothing else" in {
    val surface = rendered(
      shown(stateWith(Some(composing("XY", List(CompositionClause(0, 2, selected = true))))))
    )

    surface.styleCalls.exists(call => call.action == "enable" && call.style.isUnderlined) shouldBe true
  }
