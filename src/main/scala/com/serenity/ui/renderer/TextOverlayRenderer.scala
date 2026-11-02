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
    cellMetrics: CellMetrics,
    advances: MeasuredTextAdvances,
    window: ViewportSize = CornerRounding.UnboundedWindow
  ): Unit =
    val offsetPx = FloatingSurfaceGeometry.signedRowOffsetPixels(overlay.verticalOffsetRows, cellMetrics)
    surface.pixels.withPixelTranslation(0.0, offsetPx) {
      renderAtLogicalPixelOrigin(surface, overlay, theme, config, cursorVisible, font, cellMetrics, advances, window)
    }

  private def renderAtLogicalPixelOrigin(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    config: AppConfig,
    cursorVisible: Boolean,
    font: java.awt.Font,
    cellMetrics: CellMetrics,
    advances: MeasuredTextAdvances,
    window: ViewportSize
  ): Unit =
    val rect = overlay.rect

    val isStatusLine = overlay.surfaceId.contains(UiSurface.StatusLineSurfaceId)
    val isTabStrip   = overlay.surfaceId.contains(UiSurface.TabBarSurfaceId)
    val isQuietLine  = isStatusLine || isTabStrip
    val element =
      if isStatusLine then ChromeElement.StatusBar
      else if isTabStrip then ChromeElement.TabStrip
      else ChromeElement.FloatingSurface
    val corners = CornerRounding.corners(element, rect, window, config.dockedCornerStyle, config.paneHeaderCorners)

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

    withOptionalRoundRectClip(surface, rect, corners, config.scaledUiCornerRadiusPx) {
      for y <- rect.y until rect.bottom do
        surface.setForegroundColor(fg)
        surface.setBackgroundColor(bg)
        surface.putString(rect.x, y, " " * rect.width)

      val textInsetPx = SurfaceTextInset.px(config)
      // `composition` is `overlay`'s only content representation (issue #1683) -- a surface with genuinely nothing to
      // paint (no bespoke composition and no rows/header/footer/key-hint given at construction) simply has none, and
      // there is no separate plain-rows path left to fall back to.
      overlay.composition.foreach(
        drawComposition(
          surface,
          _,
          theme,
          cursorVisible,
          fg,
          bg,
          font,
          cellMetrics,
          advances,
          textInsetPx,
          highlightRadiusPx = if isTabStrip then config.scaledUiCornerRadiusPx else 0
        )
      )
    }
    // The status row and the tab strip are both quiet single lines, not floating panels: no border.
    if !isQuietLine then drawBorder(surface, overlay, theme, config, corners)

    surface.effects.foreach(_.setAlpha(1.0f))
    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)

  private def drawBorder(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    config: AppConfig,
    corners: RoundedCorners
  ): Unit =
    val rect = overlay.rect
    // A surface with no reserved border cell has content running to its edge, which a frame would draw over. The tab
    // bar is kept out by its caller instead: it paints edge to edge although `borderCellsFor` gives its content kind
    // the default border.
    if overlay.borderCells > 0 && rect.width >= 2 && rect.height >= 2 then
      surface.panelOutlines.foreach(
        _.strokeRoundRect(
          rect.x,
          rect.y,
          rect.width,
          rect.height,
          corners,
          config.scaledUiCornerRadiusPx,
          theme.border,
          config.scaledUiOutlineThicknessPx
        )
      )

  /** Falls back to running `render` unclipped when the surface can't clip -- content still draws. A rounded clip shapes
    * the fill like the outline, so no square corner shows outside an arc; with nothing to round it is the plain
    * rectangle clip the overlay has always had, which keeps text shifted by the inset from spilling past its edge.
    */
  private def withOptionalRoundRectClip(
    surface: RenderSurface,
    rect: LayoutRect,
    corners: RoundedCorners,
    radiusPx: Int
  )(render: => Unit): Unit =
    surface.panelOutlines match
      case Some(outlines) if corners.any && radiusPx > 0 =>
        outlines.withRoundRectClip(rect.x, rect.y, rect.width, rect.height, corners, radiusPx)(render)
      case Some(outlines) => outlines.withRectClip(rect.x, rect.y, rect.width, rect.height)(render)
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
    advances: MeasuredTextAdvances,
    textInsetPx: Double,
    highlightRadiusPx: Int
  ): Unit =
    // The new-tab box paints last: a tab's close box is shifted right by the text inset, and its background reaches
    // over the cells beside it, which would wipe out half of the glyph.
    val (newTabBoxes, otherBoxes) = composition.paintBoxes.partition(isNewTab)
    (otherBoxes ++ newTabBoxes).foreach { box =>
      box.text.foreach { text =>
        val rect  = box.rect
        val x     = math.round(rect.x).toInt
        val y     = math.round(rect.y).toInt
        val width = math.round(rect.width).toInt
        val row   = overlayRowFor(box, text, highlightRadiusPx, textInsetPx)
        def paint(): Unit = OverlayRowPainter.renderRow(
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
          advances = advances,
          // Shifted right by the inset, the new-tab glyph at the strip's last cell would run off the strip, and an
          // inactive tab's `x`, which takes that tab's last padding cell, would run past the tab into the gap.
          textInsetPx = if isNewTab(box) || isPaddingCellClose(box) then 0.0 else textInsetPx,
          // `rect.y` may carry a fractional row offset (a sub-row `itemGapRows` gap) that `y` above already rounded
          // away -- pixel positioning must come from the box's own exact offset, not from that rounded cell row, or a
          // fractional gap renders as no gap at all.
          pixelY = Some(math.round(rect.y * cellMetrics.lineHeight).toInt),
          pixelHeight = Some(math.max(1, math.round(rect.height).toInt) * cellMetrics.lineHeight)
        )
        if box.selected && highlightRadiusPx > 0 && isCloseBox(box) then
          // The active tab's close box fills a square, which would poke out past the pill's rounded right corners.
          val tabEnd = RoundedCorners(topLeft = false, topRight = true, bottomRight = true, bottomLeft = false)
          surface.panelOutlines.fold(paint())(
            _.withRoundRectClip(x, y, width + TabBarSurfaceComposition.PaddingCells, 1, tabEnd, highlightRadiusPx)(
              paint()
            )
          )
        else paint()
      }
    }

  private def isCloseBox(box: SurfacePaintBox): Boolean =
    box.focusId.exists(id => TabBarSurfaceComposition.closeBufferIdOf(id).nonEmpty)

  /** A close box that is not the active tab's: it is drawn in its tab's padding cell, with nothing after it to spill
    * into.
    */
  private def isPaddingCellClose(box: SurfacePaintBox): Boolean = isCloseBox(box) && !box.selected

  private def isNewTab(box: SurfacePaintBox): Boolean =
    box.focusId.contains(TabBarSurfaceComposition.NewTabFocusId)

  /** A box with a tone but no segments of its own paints as one implicit segment carrying that tone, so the tone goes
    * through the same selected-wins colour rules as any segment's. Explicit segments keep their own tones.
    */
  private def overlayRowFor(
    box: SurfacePaintBox,
    text: String,
    highlightRadiusPx: Int,
    textInsetPx: Double
  ): OverlayRow =
    val tonedWhole = box.segments.isEmpty && box.tone != OverlayTone.Normal
    OverlayRow(
      plainText = text,
      segmentHighlightRadiusPx = highlightRadiusPx,
      segmentInsetPx = textInsetPx,
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
