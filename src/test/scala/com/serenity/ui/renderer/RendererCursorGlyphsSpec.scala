package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.config.{AppConfig, CursorColorConfig}
import com.serenity.ui.layout.{CellMetrics, LayoutRect}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for the pure clipping/colour helpers in `RendererCursorGlyphs` (issue #1421):
  * [[RendererCursorGlyphs.caretWithin]], [[RendererCursorGlyphs.measuredRunWidthWithin]], and
  * [[RendererCursorGlyphs.cursorColorFor]]. `renderCursors` itself is exercised end-to-end by
  * `CursorPixelAlignmentSpec` and friends; these three are the pane-clipping and colour rules every one of those paint
  * paths (and `RendererMarkdownLens`'s own caret, and `RendererHighlights`'s selection clipping) ultimately shares, so
  * pinning them directly catches a regression no single integration spec would attribute correctly.
  */
class RendererCursorGlyphsSpec extends AnyFlatSpec with Matchers:

  private val font        = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val cellMetrics = CellMetrics.fromFont(font)
  private val rect        = LayoutRect(2, 0, 10, 5) // cells [2, 12)

  "caretWithin" should "place a caret already inside the pane at its desired position unmoved" in {
    val leftXPx = cellMetrics.toPixelX(rect.x)
    val result  = RendererCursorGlyphs.caretWithin(rect, cellMetrics, leftXPx + 5, desiredWidthPx = 2)

    result shouldBe Some((leftXPx + 5, 2))
  }

  it should "clamp a caret desired to the left of the pane to the pane's left edge" in {
    val leftXPx = cellMetrics.toPixelX(rect.x)
    val result  = RendererCursorGlyphs.caretWithin(rect, cellMetrics, desiredXPx = -1000, desiredWidthPx = 2)

    result shouldBe Some((leftXPx, 2))
  }

  it should "clamp a caret desired past the right edge so it still fits entirely inside the pane" in {
    val leftXPx  = cellMetrics.toPixelX(rect.x)
    val rightXPx = cellMetrics.toPixelX(rect.right)
    val result   = RendererCursorGlyphs.caretWithin(rect, cellMetrics, desiredXPx = 100000, desiredWidthPx = 3)

    result shouldBe Some((rightXPx - 3, 3))
    result.foreach { case (x, width) => (x + width) should be <= rightXPx }
    result.foreach { case (x, _) => x should be >= leftXPx }
  }

  it should "shrink a desired width wider than the whole pane down to the pane's own width" in {
    val leftXPx     = cellMetrics.toPixelX(rect.x)
    val rightXPx    = cellMetrics.toPixelX(rect.right)
    val paneWidthPx = rightXPx - leftXPx

    val result = RendererCursorGlyphs.caretWithin(rect, cellMetrics, leftXPx, desiredWidthPx = paneWidthPx * 5)

    result shouldBe Some((leftXPx, paneWidthPx))
  }

  it should "floor a non-positive desired width up to a minimum of 1 pixel rather than disappearing" in {
    val result = RendererCursorGlyphs.caretWithin(rect, cellMetrics, cellMetrics.toPixelX(rect.x), desiredWidthPx = 0)

    result.map(_._2) shouldBe Some(1)
  }

  it should "return None for a zero-width pane" in {
    val zeroWidthRect = LayoutRect(2, 0, 0, 5)
    val result =
      RendererCursorGlyphs.caretWithin(zeroWidthRect, cellMetrics, cellMetrics.toPixelX(2), desiredWidthPx = 2)

    result shouldBe None
  }

  "measuredRunWidthWithin" should "return the full run width when it fits entirely inside the pane" in {
    val leftXPx = cellMetrics.toPixelX(rect.x).toFloat

    val result = RendererCursorGlyphs.measuredRunWidthWithin(rect, contextWith(cellMetrics), leftXPx, leftXPx + 20f)

    result shouldBe Some(20f)
  }

  it should "clip a run that extends past the pane's right edge to the remaining visible width" in {
    val leftXPx  = cellMetrics.toPixelX(rect.x).toFloat
    val rightXPx = cellMetrics.toPixelX(rect.right).toFloat

    val result = RendererCursorGlyphs.measuredRunWidthWithin(rect, contextWith(cellMetrics), leftXPx, rightXPx + 500f)

    result shouldBe Some(rightXPx - leftXPx)
  }

  it should "return None for a run that starts at or past the pane's right edge" in {
    val rightXPx = cellMetrics.toPixelX(rect.right).toFloat

    val result = RendererCursorGlyphs.measuredRunWidthWithin(rect, contextWith(cellMetrics), rightXPx, rightXPx + 10f)

    result shouldBe None
  }

  it should "return None for a run whose clipped width collapses to zero" in {
    val leftXPx = cellMetrics.toPixelX(rect.x).toFloat

    val result = RendererCursorGlyphs.measuredRunWidthWithin(rect, contextWith(cellMetrics), leftXPx, leftXPx)

    result shouldBe None
  }

  "cursorColorFor" should "use the active cursor colour override when one is present, for the primary cursor" in {
    val theme         = Theme.dark
    val config        = AppConfig.default
    val overrideColor = new Color(1, 2, 3)

    RendererCursorGlyphs.cursorColorFor(
      config,
      theme,
      contextWith(cellMetrics, cursorColorOverride = Some(overrideColor)),
      isPrimaryCursor = true
    ) shouldBe overrideColor
  }

  it should "fall back to the theme's cursor colour when config has no active colour and no override applies" in {
    val theme  = Theme.dark
    val config = AppConfig.default

    RendererCursorGlyphs.cursorColorFor(config, theme, contextWith(cellMetrics), isPrimaryCursor = true) shouldBe
      config.cursorColors.activeOr(theme.cursor)
  }

  it should "colour a secondary cursor from config's inactive colour, falling back to the resolved active colour" in {
    val theme = Theme.dark
    val config = AppConfig.default.withCursorConfig(
      AppConfig.default.cursorConfig.copy(colors = CursorColorConfig(active = None, inactive = None))
    )

    val activeColor =
      RendererCursorGlyphs.cursorColorFor(config, theme, contextWith(cellMetrics), isPrimaryCursor = true)
    val secondaryColor =
      RendererCursorGlyphs.cursorColorFor(config, theme, contextWith(cellMetrics), isPrimaryCursor = false)

    secondaryColor shouldBe activeColor
  }

  it should "prefer an explicit inactive colour over the active colour for a secondary cursor" in {
    val theme    = Theme.dark
    val inactive = new Color(9, 8, 7)
    val config = AppConfig.default.withCursorConfig(
      AppConfig.default.cursorConfig.copy(colors = CursorColorConfig(active = None, inactive = Some(inactive)))
    )

    RendererCursorGlyphs.cursorColorFor(config, theme, contextWith(cellMetrics), isPrimaryCursor = false) shouldBe
      inactive
  }

  private def contextWith(
    cellMetrics: CellMetrics,
    cursorColorOverride: Option[Color] = None
  ): RenderContext =
    RenderContext(
      surface = new NoopSurface,
      layout = com.serenity.ui.layout.CalculatedLayout(
        editorPanelRect = LayoutRect(0, 0, 80, 24),
        leftSpacerRect = LayoutRect(0, 0, 0, 0),
        rightSpacerRect = LayoutRect(0, 0, 0, 0)
      ),
      cursorColorOverride = cursorColorOverride,
      codeFont = font,
      textFont = font,
      uiFont = font,
      cellMetrics = cellMetrics,
      uiMetrics = cellMetrics
    )

  /** Minimal `RenderSurface` stand-in: these three helpers never call into the surface, but `RenderContext` requires
    * one to construct.
    */
  private class NoopSurface extends RenderSurface:
    def text: TextDrawing                      = throw new UnsupportedOperationException("not exercised by these specs")
    def pixels: PixelDrawing                   = throw new UnsupportedOperationException("not exercised by these specs")
    def setForegroundColor(color: Color): Unit = ()
    def setBackgroundColor(color: Color): Unit = ()
    def getBackgroundColor: Color              = Color.BLACK
    def putString(x: Int, y: Int, s: String): Unit                          = ()
    def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit = ()
    def enableStyle(style: com.serenity.ui.theme.TextStyle): Unit           = ()
    def disableStyle(style: com.serenity.ui.theme.TextStyle): Unit          = ()
    def hideCursor(): Unit                                                  = ()
    def viewportWidth: Int                                                  = 80
    def viewportHeight: Int                                                 = 24
    def flush(): Unit                                                       = ()
