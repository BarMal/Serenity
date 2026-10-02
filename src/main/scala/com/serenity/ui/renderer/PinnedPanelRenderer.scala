package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.animation.AnimationState
import com.serenity.config.AppConfig
import com.serenity.ui.layout.{CellMetrics, ResolvedSurfaceComposition}
import com.serenity.ui.theme.Theme

object PinnedPanelRenderer:

  private val BorderAnimationColumn = -1
  private val BorderAnimationRow    = -1

  def render(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    config: AppConfig,
    cellMetrics: CellMetrics,
    animationState: AnimationState = AnimationState.empty
  ): Unit =
    val rect = panel.rect

    if config.surfaceConfig.uiShadowsEnabled then
      surface.roundedRects.foreach(
        _.drawRoundRectShadow(
          rect.x,
          rect.y,
          rect.width,
          rect.height,
          config.scaledUiCornerRadiusPx,
          new java.awt.Color(0, 0, 0)
        )
      )
    val backdrop        = SurfaceMaterials.backdropShowingThrough(config, theme, surface)
    val panelBackground = backdrop.getOrElse(theme.panel.background)
    surface.effects.foreach(_.setAlpha(SurfaceMaterials.panelAlpha(config, theme)))
    surface.setForegroundColor(theme.panel.foreground)
    surface.setBackgroundColor(panelBackground)

    for y <- rect.y until rect.bottom do surface.putString(rect.x, y, " " * rect.width)

    val textInsetPx = SurfaceTextInset.px(config)
    if backdrop.isEmpty then applyGlassSheen(surface, panel, theme, config)
    drawBorder(surface, panel, theme, config, animationState)
    drawTitle(surface, panel, theme, animationState, textInsetPx)
    drawComposition(surface, panel, panel.composition, theme, panelBackground, animationState, cellMetrics, textInsetPx)

    surface.effects.foreach(_.setAlpha(1.0f))
    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)

  private def drawBorder(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    config: AppConfig,
    animationState: AnimationState
  ): Unit =
    val rect = panel.rect
    if rect.width >= 2 && rect.height >= 2 then
      val borderColor =
        animationForeground(animationState, BorderAnimationColumn, BorderAnimationRow).getOrElse(theme.border)
      surface.roundedRects.foreach(
        _.strokeRoundRect(
          rect.x,
          rect.y,
          rect.width,
          rect.height,
          config.scaledUiCornerRadiusPx,
          borderColor,
          config.scaledUiOutlineThicknessPx
        )
      )

  private def drawTitle(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    animationState: AnimationState,
    textInsetPx: Double
  ): Unit =
    val titleRect = panel.titleRect
    // The title sits on the frame's top edge, so it is not padded out: padding would blank the rest of that edge.
    val title = panel.title.take(titleRect.width)
    if titleRect.width > 0 then
      surface.pixels.withPixelTranslation(textInsetPx, 0.0) {
        renderAnimatedText(surface, titleRect.x, titleRect.y, title, 0, theme.panel.foreground, animationState)
      }

  /** Paints a panel's composed plan -- box rects are already whole-cell granular (built from the same integer
    * content-rect coordinates `contentRowSlotsFor` uses), so no font-metric or sub-cell mapping is needed, unlike
    * `TextOverlayRenderer.drawComposition`'s pixel-measured floating layout.
    */
  private def drawComposition(
    surface: RenderSurface,
    panel: TextPanelView,
    composition: ResolvedSurfaceComposition,
    theme: Theme,
    panelBackground: Color,
    animationState: AnimationState,
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
          renderAnimatedText(surface, x, y, padded, y - panel.rect.y, foreground, animationState)
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
    color: Color
  ): Unit =
    val leftPx  = cellMetrics.toPixelX(x)
    val rightPx = cellMetrics.toPixelX(x + width)
    surface.pixels.fillPixelRect(leftPx, cellMetrics.toPixelY(y), rightPx - leftPx, cellMetrics.lineHeight, color)

  private def renderAnimatedText(
    surface: RenderSurface,
    x: Int,
    y: Int,
    text: String,
    animationRow: Int,
    defaultForeground: java.awt.Color,
    animationState: AnimationState
  ): Unit =
    text.zipWithIndex.foreach { (char, index) =>
      surface.setForegroundColor(animationForeground(animationState, index, animationRow).getOrElse(defaultForeground))
      CharacterRenderer.renderChar(surface, x + index, y, char)
    }

  private def animationForeground(animationState: AnimationState, column: Int, row: Int): Option[java.awt.Color] =
    animationState.getCell(column, row).flatMap(_.currentForeground)

  private def applyGlassSheen(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    config: AppConfig
  ): Unit =
    SurfaceMaterials.glassSheenBackground(config, theme).foreach { sheenColor =>
      val contentRect = panel.resolvedContentRect
      val sheenWidth  = contentRect.width
      val sheenHeight = math.min(1, contentRect.height)
      if sheenWidth > 0 && sheenHeight > 0 then
        surface.setBackgroundColor(sheenColor)
        (0 until sheenHeight).foreach { rowOffset =>
          CharacterRenderer.renderStringPlain(surface, contentRect.x, contentRect.y + rowOffset, " " * sheenWidth)
        }
    }
