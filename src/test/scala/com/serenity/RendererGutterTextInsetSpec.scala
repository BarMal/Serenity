package com.serenity

import java.awt.Font

import com.serenity.config.{LineNumberLayout, LineNumberSide}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{RendererEntryPoints, SurfaceTextInset}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1542 (pixel-precision follow-up): the line-number gutter's margin (the gap between the pane edge and the
  * counter) previously sat wherever `LayoutEngine`'s whole-cell reservation happened to put it -- the same
  * whole-cell-only limitation `SurfaceTextInset` already fixed for panel text. `LayoutEngine`/`EditorLayoutContract`
  * stay on the shared cell grid (hit-testing, drag-resize and the TUI all still need it -- see
  * `GutterDividerRenderSpec`/`GutterDividerContinuousLineSpec`, both unaffected by this), but the GUI's *paint* of the
  * counter digits now nudges them a sub-cell amount further from the pane edge, via the same `SurfaceTextInset` value
  * every other piece of framed chrome uses -- entirely inside the cell `LayoutEngine` already reserved, so it never
  * collides with anything else on that reserved grid.
  *
  * Only exercised on the measured (pixel-run) drawing path -- `RendererGutter.useMeasuredLineNumberFont` -- since a
  * genuinely monospaced code font already draws line numbers cell-perfect via `putString` and a terminal surface (no
  * `FontRenderContext`) never takes this path at all, so TUI is untouched by construction.
  */
class RendererGutterTextInsetSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
  // Not monospaced (i/m/W widths differ for a real serif font), so `TextLayoutSnapshot.shouldUseMeasuredLayout`
  // routes this buffer's line numbers onto the pixel-run `drawRunPx` path instead of the whole-cell `putString` one.
  private val textFont = Font(Font.SERIF, Font.PLAIN, 12)

  private def stateWithBuffer(lineNumberLayout: LineNumberLayout = LineNumberLayout()): AppState =
    val lines = (1 to 5).map(i => s"line $i").mkString("\n")
    // Default `Document.language` is `None`, which resolves to `TypographyRole.Prose` -- a buffer whose font
    // (`textFont`) differs from `codeFont`, so `RendererGutter.useMeasuredLineNumberFont` is satisfied.
    val buffer = Buffer
      .fromString(BufferId(1), lines)
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 10, visibleColumns = 20))
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
        ),
        theme = Theme.light,
        config = AppState.initial.persisted.config.withLineNumberLayout(lineNumberLayout)
      )
    )

  private def render(state: AppState, surface: MockRenderSurface): Unit =
    val viewport    = ViewportSize(80, 24)
    val cellMetrics = CellMetrics.fromFont(codeFont)
    RendererEntryPoints.render(state, cursorVisible = false, surface, viewport, codeFont, textFont, cellMetrics, None)

  "a left-placed line-number counter" should "inset its digits a sub-cell amount from the pane edge, toward the divider" in {
    val state   = stateWithBuffer()
    val surface = new MockRenderSurface(80, 24)
    render(state, surface)

    surface.drawRunPxCalls should not be empty
    val insetPx = SurfaceTextInset.px(state.persisted.config)
    insetPx should be > 0.0

    surface.pixelTranslationCalls.map(_.xPx) should contain(insetPx)
  }

  it should "inset by less than one cell, so the digits stay inside the cell LayoutEngine already reserved" in {
    val state       = stateWithBuffer()
    val surface     = new MockRenderSurface(80, 24)
    val cellMetrics = CellMetrics.fromFont(codeFont)
    render(state, surface)

    SurfaceTextInset.px(state.persisted.config) should be < cellMetrics.charWidth.toDouble
  }

  "a right-placed line-number counter" should "inset its digits toward the divider, i.e. away from the pane's outer edge" in {
    val state   = stateWithBuffer(LineNumberLayout(side = LineNumberSide.Right))
    val surface = new MockRenderSurface(80, 24)
    render(state, surface)

    surface.drawRunPxCalls should not be empty
    val insetPx = SurfaceTextInset.px(state.persisted.config)

    surface.pixelTranslationCalls.map(_.xPx) should contain(-insetPx)
  }

  it should "not apply the gutter's margin inset to a code buffer, which already draws line numbers cell-perfect" in {
    val lines = (1 to 5).map(i => s"line $i").mkString("\n")
    val plain = Buffer.fromString(BufferId(1), lines)
    val buffer = plain.copy(
      document = plain.document.copy(language = Some(com.serenity.lsp.config.LanguageId.Scala)),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 10, visibleColumns = 20)
    )
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
        ),
        theme = Theme.light
      )
    )
    val surface = new MockRenderSurface(80, 24)
    render(state, surface)

    val insetPx = SurfaceTextInset.px(state.persisted.config)
    surface.pixelTranslationCalls.map(_.xPx) should not contain insetPx
  }
