package com.serenity.ui.renderer

import com.serenity.config.AppConfig
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{
  CellMetrics,
  ChromeElement,
  CornerRounding,
  ResolvedSurfaceComposition,
  RoundedCorners,
  ViewportSize
}
import com.serenity.ui.theme.Theme

object PinnedPanelRenderer:

  /** What a surface that draws proportional text needs to set a panel's rows as runs of letters rather than one letter
    * to a cell: the font they are drawn in and the cache of its measured advances.
    */
  final case class PanelTextMeasure(font: java.awt.Font, advances: MeasuredTextAdvances)

  def render(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    config: AppConfig,
    cellMetrics: CellMetrics,
    window: ViewportSize = CornerRounding.UnboundedWindow,
    measure: Option[PanelTextMeasure] = None
  ): Unit =
    val rect            = panel.rect
    val panelBackground = theme.panel.background
    val corners =
      CornerRounding.corners(
        ChromeElement.DockedPanel,
        rect,
        window,
        config.dockedCornerStyle,
        config.paneHeaderCorners
      )

    surface.setForegroundColor(theme.panel.foreground)
    surface.setBackgroundColor(panelBackground)
    val fill = () => for y <- rect.y until rect.bottom do surface.putString(rect.x, y, " " * rect.width)
    val roundedFill = surface.panelOutlines.exists(
      _.fillRoundRect(rect.x, rect.y, rect.width, rect.height, corners, config.scaledUiCornerRadiusPx, panelBackground)
    )
    if !roundedFill then fill()

    val textInsetPx = SurfaceTextInset.px(config)
    drawBorder(surface, panel, theme, config, corners)
    drawTitle(surface, panel, theme, cellMetrics, textInsetPx, measure)
    drawComposition(surface, panel.composition, theme, panelBackground, cellMetrics, textInsetPx, measure)

    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)

  private def drawBorder(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    config: AppConfig,
    corners: RoundedCorners
  ): Unit =
    val rect = panel.rect
    if rect.width >= 2 && rect.height >= 2 then
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

  private def drawTitle(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    cellMetrics: CellMetrics,
    textInsetPx: Double,
    measure: Option[PanelTextMeasure]
  ): Unit =
    val titleRect = panel.titleRect
    // The title sits on the frame's top edge, so it is not padded out: padding would blank the rest of that edge.
    val title = panel.title.take(titleRect.width)
    if titleRect.width > 0 then
      surface.pixels.withPixelTranslation(textInsetPx, 0.0) {
        renderText(
          surface,
          titleRect.x,
          titleRect.y,
          titleRect.width,
          title,
          theme.panel.foreground,
          cellMetrics,
          textInsetPx,
          measure
        )
      }

  /** Paints a panel's composed plan -- box rects are already whole-cell granular (built from the same integer
    * content-rect coordinates `contentRowSlotsFor` uses), so no font-metric or sub-cell mapping is needed, unlike
    * `TextOverlayRenderer.drawComposition`'s pixel-measured floating layout.
    */
  private def drawComposition(
    surface: RenderSurface,
    composition: ResolvedSurfaceComposition,
    theme: Theme,
    panelBackground: RenderColor,
    cellMetrics: CellMetrics,
    textInsetPx: Double,
    measure: Option[PanelTextMeasure]
  ): Unit =
    composition.paintBoxes.foreach { box =>
      box.text.foreach { text =>
        val x      = math.round(box.rect.x).toInt
        val y      = math.round(box.rect.y).toInt
        val width  = math.round(box.rect.width).toInt
        val padded = text.take(width).padTo(width, ' ')
        val (foreground, background) =
          if box.selected then (theme.highlighted.foreground, theme.highlighted.background)
          else
            (
              OverlaySegmentRowRenderer.toneForeground(box.tone, theme).getOrElse(theme.panel.foreground),
              OverlaySegmentRowRenderer.toneBackground(box.tone, theme).getOrElse(panelBackground)
            )
        surface.setForegroundColor(foreground)
        surface.setBackgroundColor(background)
        if box.selected then surface.enableStyle(theme.focusStyle)
        if box.selected || (measure.nonEmpty && background != panelBackground) then
          fillRowBackground(surface, cellMetrics, x, y, width, background)
        surface.pixels.withPixelTranslation(textInsetPx, 0.0) {
          renderText(surface, x, y, width, padded, foreground, cellMetrics, textInsetPx, measure)
        }
        if box.selected then surface.disableStyle(theme.focusStyle)
      }
    }

  /** Fills one row's full content width with `color`, at its true (un-inset) position -- called only for a selected
    * row/box, ahead of its glyphs drawing shifted by `textInsetPx`. Every other row already gets this from `render`'s
    * own full-panel blank, which already paints the unselected panel background; a selected row needs its own fill
    * because that blank predates knowing which row is selected.
    */
  private def fillRowBackground(
    surface: RenderSurface,
    cellMetrics: CellMetrics,
    x: Int,
    y: Int,
    width: Int,
    color: RenderColor
  ): Unit =
    val leftPx  = cellMetrics.toPixelX(x)
    val rightPx = cellMetrics.toPixelX(x + width)
    surface.pixels.fillPixelRect(
      leftPx,
      cellMetrics.toPixelY(y),
      rightPx - leftPx,
      cellMetrics.lineHeight,
      color
    )

  /** On a surface that can measure text the row is one run of letters at its natural widths, cut at the last letter
    * that fits the row; a letter to a cell would leave gaps inside words and let a wide letter's cell wipe the one
    * before it. A cell-grid surface, which has no proportional text, still sets a letter to a cell.
    */
  private def renderText(
    surface: RenderSurface,
    x: Int,
    y: Int,
    widthCells: Int,
    text: String,
    foreground: RenderColor,
    cellMetrics: CellMetrics,
    textInsetPx: Double,
    measure: Option[PanelTextMeasure]
  ): Unit =
    surface.setForegroundColor(foreground)
    val measured = for
      m   <- measure
      frc <- surface.text.fontRenderContext
    yield (m, frc)
    measured match
      case Some((m, frc)) =>
        val availablePx = (widthCells * cellMetrics.charWidth - math.round(textInsetPx).toInt).max(1)
        val fitted      = fitting(text.stripTrailing(), availablePx, m, frc)
        if fitted.nonEmpty then
          surface.text.drawRunPx(
            cellMetrics.toPixelX(x).toFloat,
            cellMetrics.toPixelY(y),
            m.advances.advanceOf(fitted, m.font, frc).max(1.0f),
            cellMetrics.lineHeight,
            cellMetrics.ascent,
            fitted
          )
      case None =>
        text.zipWithIndex.foreach((char, index) => CharacterRenderer.renderChar(surface, x + index, y, char))

  /** The longest start of `text` whose width is at most `availablePx`. */
  private def fitting(
    text: String,
    availablePx: Int,
    measure: PanelTextMeasure,
    frc: java.awt.font.FontRenderContext
  ): String =
    def width(length: Int): Float = measure.advances.advanceOf(text.take(length), measure.font, frc)
    if width(text.length) <= availablePx then text
    else
      @annotation.tailrec
      def search(low: Int, high: Int): Int =
        if low >= high then low
        else
          val middle = (low + high + 1) / 2
          if width(middle) <= availablePx then search(middle, high) else search(low, middle - 1)
      text.take(search(0, text.length))
