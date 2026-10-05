package com.serenity.ui.renderer

import java.awt.Font

import scala.util.chaining.*

import com.serenity.config.AppConfig
import com.serenity.state.models.UiSurface
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme

object TextOverlayRenderer:

  def render(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    config: AppConfig,
    cursorVisible: Boolean,
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val offsetPx = FloatingSurfaceGeometry.signedRowOffsetPixels(overlay.verticalOffsetRows, cellMetrics)
    surface.pixels.withPixelTranslation(0.0, offsetPx) {
      renderAtLogicalPixelOrigin(surface, overlay, theme, config, cursorVisible, font, cellMetrics)
    }

  private def renderAtLogicalPixelOrigin(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    config: AppConfig,
    cursorVisible: Boolean,
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val rect = overlay.rect

    val isStatusLine = overlay.surfaceId.contains(UiSurface.StatusLineSurfaceId)
    val isQuietLine  = isStatusLine || overlay.surfaceId.contains(UiSurface.TabBarSurfaceId)

    // Scoped to the one surface the status line's colour overrides name -- every other floating panel keeps painting
    // with the theme's panel colours and alpha exactly as before, unmodified (#1295).
    val statusColors = config.statusLine.colors
    val statusBackgroundAlphaOverride: Option[Int] =
      Option
        .when(isStatusLine)(statusColors.backgroundAlpha)
        .flatten
        .map(alpha => math.round(alpha * 255.0).toInt.max(0).min(255))
    val statusForegroundOverride: Option[RenderColor] =
      Option.when(isStatusLine)(statusColors.foreground).flatten.map(RenderColor.fromAwt)
    val statusBackgroundOverride: Option[RenderColor] =
      Option.when(isStatusLine)(statusColors.background).flatten.map(RenderColor.fromAwt)

    val fg = statusForegroundOverride.getOrElse(theme.panel.foreground)
    val bg = statusBackgroundOverride
      .getOrElse(theme.panel.background)
      .pipe(color => statusBackgroundAlphaOverride.fold(color)(color.withAlpha))

    surface.effects.foreach(_.setAlpha(overlay.alphaMultiplier))

    withOptionalRectClip(surface, rect.x, rect.y, rect.width, rect.height) {
      for y <- rect.y until rect.bottom do
        surface.setForegroundColor(fg)
        surface.setBackgroundColor(bg)
        surface.putString(rect.x, y, " " * rect.width)

      val textInsetPx = SurfaceTextInset.px(config)
      // `composition` is `overlay`'s only content representation (issue #1683) -- a surface with genuinely nothing to
      // paint (no bespoke composition and no rows/header/footer/key-hint given at construction) simply has none, and
      // there is no separate plain-rows path left to fall back to.
      overlay.composition.foreach(
        drawComposition(surface, _, theme, cursorVisible, fg, bg, font, cellMetrics, textInsetPx)
      )
    }
    // The status row and the tab strip are both quiet single lines, not floating panels: no border.
    if !isQuietLine then drawBorder(surface, overlay, theme, config)

    surface.effects.foreach(_.setAlpha(1.0f))
    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)

  private def drawBorder(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    config: AppConfig
  ): Unit =
    val rect = overlay.rect
    // A surface with no reserved border cell has content running to its edge, which a frame would draw over. The tab
    // bar is kept out by its caller instead: it paints edge to edge although `borderCellsFor` gives its content kind
    // the default border.
    if overlay.borderCells > 0 && rect.width >= 2 && rect.height >= 2 then
      surface.panelOutlines.foreach(
        _.strokeRect(
          rect.x,
          rect.y,
          rect.width,
          rect.height,
          theme.border,
          config.scaledUiOutlineThicknessPx
        )
      )

  /** Falls back to running `render` unclipped when the surface can't clip -- content still draws. */
  private def withOptionalRectClip(surface: RenderSurface, x: Int, y: Int, width: Int, height: Int)(
    render: => Unit
  ): Unit =
    surface.panelOutlines match
      case Some(outlines) => outlines.withRectClip(x, y, width, height)(render)
      case None           => render

  private def drawComposition(
    surface: RenderSurface,
    composition: ResolvedSurfaceComposition,
    theme: Theme,
    cursorVisible: Boolean,
    fg: RenderColor,
    bg: RenderColor,
    font: Font,
    cellMetrics: CellMetrics,
    textInsetPx: Double
  ): Unit =
    composition.paintBoxes.foreach { box =>
      box.text.foreach { text =>
        val rect  = box.rect
        val x     = math.round(rect.x).toInt
        val y     = math.round(rect.y).toInt
        val width = math.round(rect.width).toInt
        val row   = overlayRowFor(box, text)
        OverlayRowPainter.renderRow(
          surface,
          x,
          y,
          width,
          row,
          theme,
          cursorVisible,
          defaultForeground = Some(fg),
          defaultBackground = Some(bg),
          font = font,
          cellMetrics = cellMetrics,
          textInsetPx = textInsetPx,
          // `rect.y` may carry a fractional row offset (a sub-row `itemGapRows` gap) that `y` above already rounded
          // away -- pixel positioning must come from the box's own exact offset, not from that rounded cell row, or a
          // fractional gap renders as no gap at all.
          pixelY = Some(math.round(rect.y * cellMetrics.lineHeight).toInt),
          pixelHeight = Some(math.max(1, math.round(rect.height).toInt) * cellMetrics.lineHeight)
        )
      }
    }

  /** A box with a tone but no segments of its own paints as one implicit segment carrying that tone, so the tone goes
    * through the same selected-wins colour rules as any segment's. Explicit segments keep their own tones.
    */
  private def overlayRowFor(box: SurfacePaintBox, text: String): OverlayRow =
    val tonedWhole = box.segments.isEmpty && box.tone != OverlayTone.Normal
    OverlayRow(
      plainText = text,
      selected = box.selected,
      cursorColumn = box.cursorOffset,
      segments =
        if tonedWhole then List(OverlaySegment(text, selected = box.selected, tone = box.tone)) else box.segments,
      layout =
        if tonedWhole then OverlayRowLayout.Plain
        else
          box.layout match
            case SurfacePaintLayout.Plain       => OverlayRowLayout.Plain
            case SurfacePaintLayout.Split       => OverlayRowLayout.Split
            case SurfacePaintLayout.Inline      => OverlayRowLayout.Plain
            case SurfacePaintLayout.Columns     => OverlayRowLayout.Columns
            case SurfacePaintLayout.Distributed => OverlayRowLayout.Distributed
    )
