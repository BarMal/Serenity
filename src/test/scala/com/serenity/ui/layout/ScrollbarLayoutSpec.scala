package com.serenity.ui.layout

import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The strip an editor pane's scrollbar lies in: over the content's right edge, taking nothing from it. */
class ScrollbarLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewportSize = ViewportSize(100, 30)
  private val metrics      = CellMetrics(charWidth = 9, lineHeight = 20, ascent = 15)

  private def paneLayout(state: AppState, size: ViewportSize = viewportSize): EditorPaneLayout =
    val layout = LayoutEngine.calculateLayout(state, size)
    LayoutEngine.calculateEditorPaneLayouts(state, layout)(PaneId(0))

  private def withSurface(state: AppState)(
    change: com.serenity.config.SurfaceConfig => com.serenity.config.SurfaceConfig
  ) =
    val config = state.persisted.config
    state.copy(persisted = state.persisted.copy(config = config.copy(surfaceConfig = change(config.surfaceConfig))))

  "A pane's scrollbar" should "lie over the right edge of the content, ending where the content ends" in {
    val pane  = paneLayout(AppState.initial)
    val strip = ScrollbarLayout.strip(pane.contentRect, metrics)

    strip.rightPx shouldBe metrics.toPixelX(pane.contentRect.right)
    strip.widthPx shouldBe ScrollbarLayout.StripWidthPx
    strip.xPx should be >= metrics.toPixelX(pane.contentRect.right - 1)
  }

  it should "run beside the content for its whole height" in {
    val pane  = paneLayout(AppState.initial)
    val strip = ScrollbarLayout.strip(pane.contentRect, metrics)

    strip.yPx shouldBe metrics.toPixelY(pane.contentRect.y)
    strip.bottomPx shouldBe metrics.toPixelY(pane.contentRect.bottom)
  }

  it should "never be wider than the cell it lies over" in {
    val pane   = paneLayout(AppState.initial)
    val narrow = CellMetrics(charWidth = 4, lineHeight = 20, ascent = 15)

    ScrollbarLayout.strip(pane.contentRect, narrow).widthPx shouldBe 4
  }

  it should "take no width from the text, which wraps where it always did" in {
    val state  = AppState.initial
    val layout = LayoutEngine.calculateLayout(state, viewportSize)
    val synced = LayoutEngine.syncViewportDimensions(state, viewportSize)
    val buffer = synced.persisted.buffers.values.headOption.getOrElse(fail("expected a buffer"))

    paneLayout(state).contentRect.width shouldBe layout.editorPanelRect.width
    buffer.viewport.visibleColumns shouldBe layout.editorPanelRect.width
  }

  it should "be shown on a pane of the GUI" in {
    ScrollbarLayout.shown(AppState.initial, paneLayout(AppState.initial).contentRect) shouldBe true
  }

  it should "be absent from a terminal, which cannot draw anything thinner than a cell" in {
    val tui = AppState.initial.copy(runtime = AppState.initial.runtime.copy(capabilities = FrontendCapabilities.tui()))

    ScrollbarLayout.shown(tui, paneLayout(tui).contentRect) shouldBe false
  }

  it should "be absent while text is laid out in pages of columns" in {
    val paged = withSurface(AppState.initial)(_.copy(columnModeEnabled = true, wordWrapEnabled = true))

    ScrollbarLayout.shown(paged, paneLayout(paged).contentRect) shouldBe false
  }

  it should "be absent from a pane too narrow to carry one" in {
    val narrow = ViewportSize(8, 30)

    ScrollbarLayout.shown(AppState.initial, paneLayout(AppState.initial, narrow).contentRect) shouldBe false
  }
