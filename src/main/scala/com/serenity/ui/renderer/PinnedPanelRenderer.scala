package com.serenity.ui.renderer

import com.serenity.config.AppConfig
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, ResolvedSurfaceComposition}
import com.serenity.ui.theme.Theme

object PinnedPanelRenderer:

  def render(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    config: AppConfig,
    cellMetrics: CellMetrics
  ): Unit =
    val rect            = panel.rect
    val panelBackground = theme.panel.background

    surface.setForegroundColor(theme.panel.foreground)
    surface.setBackgroundColor(panelBackground)
    for y <- rect.y until rect.bottom do surface.putString(rect.x, y, " " * rect.width)

    val textInsetPx = SurfaceTextInset.px(config)
    drawBorder(surface, panel, theme, config)
    drawTitle(surface, panel, theme, textInsetPx)
    drawComposition(surface, panel.composition, theme, panelBackground, cellMetrics, textInsetPx)

    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)

  private def drawBorder(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    config: AppConfig
  ): Unit =
    val rect = panel.rect
    if rect.width >= 2 && rect.height >= 2 then
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

  private def drawTitle(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    textInsetPx: Double
  ): Unit =
    val titleRect = panel.titleRect
    // The title sits on the frame's top edge, so it is not padded out: padding would blank the rest of that edge.
    val title = panel.title.take(titleRect.width)
    if titleRect.width > 0 then
      surface.pixels.withPixelTranslation(textInsetPx, 0.0) {
        renderText(surface, titleRect.x, titleRect.y, title, theme.panel.foreground)
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
    textInsetPx: Double
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
        if box.selected then
          surface.enableStyle(theme.focusStyle)
          fillRowBackground(surface, cellMetrics, x, y, width, background)
        surface.pixels.withPixelTranslation(textInsetPx, 0.0) {
          renderText(surface, x, y, padded, foreground)
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

  private def renderText(surface: RenderSurface, x: Int, y: Int, text: String, foreground: RenderColor): Unit =
    surface.setForegroundColor(foreground)
    text.zipWithIndex.foreach((char, index) => CharacterRenderer.renderChar(surface, x + index, y, char))
