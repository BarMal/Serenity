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
    val pane  = paneLayout(AppState.initial(com.serenity.config.AppConfig.conventional))
    val strip = ScrollbarLayout.strip(pane.contentRect, metrics)

    strip.rightPx shouldBe metrics.toPixelX(pane.contentRect.right)
    strip.widthPx shouldBe ScrollbarLayout.StripWidthPx
    strip.xPx should be >= metrics.toPixelX(pane.contentRect.right - 1)
  }

  it should "run beside the content for its whole height" in {
    val pane  = paneLayout(AppState.initial(com.serenity.config.AppConfig.conventional))
    val strip = ScrollbarLayout.strip(pane.contentRect, metrics)

    strip.yPx shouldBe metrics.toPixelY(pane.contentRect.y)
    strip.bottomPx shouldBe metrics.toPixelY(pane.contentRect.bottom)
  }

  it should "never be wider than the cell it lies over" in {
    val pane   = paneLayout(AppState.initial(com.serenity.config.AppConfig.conventional))
    val narrow = CellMetrics(charWidth = 4, lineHeight = 20, ascent = 15)

    ScrollbarLayout.strip(pane.contentRect, narrow).widthPx shouldBe 4
  }

  it should "take no width from the text, which wraps where it always did" in {
    val state  = AppState.initial(com.serenity.config.AppConfig.conventional)
    val layout = LayoutEngine.calculateLayout(state, viewportSize)
    val synced = LayoutEngine.syncViewportDimensions(state, viewportSize)
    val buffer = synced.persisted.buffers.values.headOption.getOrElse(fail("expected a buffer"))

    paneLayout(state).contentRect.width shouldBe layout.editorPanelRect.width
    buffer.viewport.visibleColumns shouldBe layout.editorPanelRect.width
  }

  it should "be shown on a pane of the GUI" in {
    ScrollbarLayout.shown(
      AppState.initial(com.serenity.config.AppConfig.conventional),
      paneLayout(AppState.initial(com.serenity.config.AppConfig.conventional)).contentRect
    ) shouldBe true
  }

  it should "not be a strip over a terminal's text, which cannot draw anything thinner than a cell" in {
    val tui = AppState
      .initial(com.serenity.config.AppConfig.conventional)
      .copy(runtime =
        AppState
          .initial(com.serenity.config.AppConfig.conventional)
          .runtime
          .copy(capabilities = FrontendCapabilities.tui())
      )

    ScrollbarLayout.shown(tui, paneLayout(tui).contentRect) shouldBe false
  }

  "A pane with the scrollbar turned off" should "have no overlay strip in the GUI" in {
    val off =
      withSurface(AppState.initial(com.serenity.config.AppConfig.conventional))(_.copy(scrollbarEnabled = false))

    ScrollbarLayout.shown(off, paneLayout(off).contentRect) shouldBe false
    ScrollbarLayout.hasBar(off, paneLayout(off)) shouldBe false
  }

  it should "keep its whole content band for text in a terminal" in {
    val off  = withSurface(tui)(_.copy(scrollbarEnabled = false))
    val pane = paneLayout(off)

    pane.scrollbarRect.width shouldBe 0
    pane.contentRect.width shouldBe paneLayout(tui).contentRect.width + 1
    ScrollbarLayout.hasBar(off, pane) shouldBe false
  }

  private val tui =
    AppState
      .initial(com.serenity.config.AppConfig.conventional)
      .copy(runtime =
        AppState
          .initial(com.serenity.config.AppConfig.conventional)
          .runtime
          .copy(capabilities = FrontendCapabilities.tui())
      )

  "A terminal pane" should "give the last column of its content band to the bar" in {
    val gui  = paneLayout(AppState.initial(com.serenity.config.AppConfig.conventional))
    val pane = paneLayout(tui)

    val panel = LayoutEngine.calculateLayout(tui, viewportSize).editorPanelRect

    pane.scrollbarRect.width shouldBe ScrollbarLayout.terminalColumns(tui, panel.width)
    pane.scrollbarRect.width shouldBe 1
    pane.contentRect.width shouldBe panel.width - 1
    pane.scrollbarRect.x shouldBe pane.contentRect.right
    pane.scrollbarRect.right shouldBe panel.right
  }

  it should "run the bar beside the content for its whole height" in {
    val pane = paneLayout(tui)

    pane.scrollbarRect.y shouldBe pane.contentRect.y
    pane.scrollbarRect.height shouldBe pane.contentRect.height
    ScrollbarLayout.hasBar(tui, pane) shouldBe true
  }

  it should "wrap its text one column short, so the viewport is one column narrower" in {
    val synced = LayoutEngine.syncViewportDimensions(tui, viewportSize)
    val panel  = LayoutEngine.calculateLayout(tui, viewportSize).editorPanelRect
    val buffer = synced.persisted.buffers.values.headOption.getOrElse(fail("expected a buffer"))

    buffer.viewport.visibleColumns shouldBe panel.width - 1
  }

  it should "give no column to a pane too narrow to carry a bar, nor to pages of columns" in {
    val paged = withSurface(tui)(_.copy(columnModeEnabled = true, wordWrapEnabled = true))

    paneLayout(tui, ViewportSize(8, 30)).scrollbarRect.width shouldBe 0
    paneLayout(paged).scrollbarRect.width shouldBe 0
  }

  it should "stay out of the GUI, where the bar is an overlay" in {
    paneLayout(AppState.initial(com.serenity.config.AppConfig.conventional)).scrollbarRect.width shouldBe 0
    ScrollbarLayout.hasBar(
      AppState.initial(com.serenity.config.AppConfig.conventional),
      paneLayout(AppState.initial(com.serenity.config.AppConfig.conventional))
    ) shouldBe true
  }

  it should "be absent while text is laid out in pages of columns" in {
    val paged = withSurface(AppState.initial(com.serenity.config.AppConfig.conventional))(
      _.copy(columnModeEnabled = true, wordWrapEnabled = true)
    )

    ScrollbarLayout.shown(paged, paneLayout(paged).contentRect) shouldBe false
  }

  it should "be absent from a pane too narrow to carry one" in {
    val narrow = ViewportSize(8, 30)

    ScrollbarLayout.shown(
      AppState.initial(com.serenity.config.AppConfig.conventional),
      paneLayout(AppState.initial(com.serenity.config.AppConfig.conventional), narrow).contentRect
    ) shouldBe false
  }
