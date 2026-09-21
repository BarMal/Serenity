package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.animation.AnimationState
import com.serenity.config.AppConfig
import com.serenity.ui.layout.{CellMetrics, ResolvedSurfaceComposition, SurfaceContentRowKind}
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
    surface.effects.foreach(_.setAlpha(SurfaceMaterials.panelAlpha(config, theme)))
    surface.setForegroundColor(theme.panel.foreground)
    surface.setBackgroundColor(theme.panel.background)

    for y <- rect.y until rect.bottom do surface.putString(rect.x, y, " " * rect.width)

    val textInsetPx = SurfaceTextInset.px(config)
    applyGlassSheen(surface, panel, theme, config)
    drawBorder(surface, panel, theme, config, animationState)
    drawTitle(surface, panel, theme, animationState, textInsetPx)
    panel.composition match
      case Some(composition) =>
        drawComposition(surface, panel, composition, theme, animationState, cellMetrics, textInsetPx)
      case None =>
        drawLines(surface, panel, theme, animationState, cellMetrics, textInsetPx)

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
    val title     = panel.title.take(titleRect.width).padTo(titleRect.width, ' ')
    if titleRect.width > 0 then
      surface.pixels.withPixelTranslation(textInsetPx, 0.0) {
        renderAnimatedText(surface, titleRect.x, titleRect.y, title, 0, theme.panel.foreground, animationState)
      }

  private def drawLines(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    animationState: AnimationState,
    cellMetrics: CellMetrics,
    textInsetPx: Double
  ): Unit =
    val contentRect = panel.resolvedContentRect
    val maxLineSize = contentRect.width
    panel.contentRowSlots
      .foreach { slot =>
        val maybeRow = slot.kind match
          case SurfaceContentRowKind.Header      => panel.header
          case SurfaceContentRowKind.Item(index) => panel.rows.lift(index)
          case SurfaceContentRowKind.Footer      => panel.footer
          // Pinned panels never populate a key-hint row today (issue #931, Stage 3's persistent footer is
          // command-palette/settings-surface-only, and those never pin) -- `PinnedPanelViewModel.contentRowSlots`
          // never asks `contentRowSlotsFor` for a `KeyHint` slot, so this is unreachable in practice.
          case SurfaceContentRowKind.KeyHint => None

        maybeRow.foreach { row =>
          val padded = row.plainText.take(maxLineSize).padTo(maxLineSize, ' ')
          val (foreground, background) =
            if row.selected then (theme.highlighted.foreground, theme.highlighted.background)
            else (theme.panel.foreground, theme.panel.background)
          surface.setForegroundColor(foreground)
          surface.setBackgroundColor(background)
          if row.selected then
            surface.enableStyle(theme.focusStyle)
            fillRowBackground(surface, cellMetrics, contentRect.x, slot.y, maxLineSize, background)
          surface.pixels.withPixelTranslation(textInsetPx, 0.0) {
            renderAnimatedText(
              surface,
              contentRect.x,
              slot.y,
              padded,
              slot.y - panel.rect.y,
              foreground,
              animationState
            )
          }
          if row.selected then surface.disableStyle(theme.focusStyle)
        }
      }

  /** Paints a composed plan's boxes in place of `drawLines`'s plain rows -- box rects are already whole-cell granular
    * (built from the same integer content-rect coordinates `contentRowSlotsFor` uses), so no font-metric or sub-cell
    * mapping is needed, unlike `TextOverlayRenderer.drawComposition`'s pixel-measured floating layout.
    */
  private def drawComposition(
    surface: RenderSurface,
    panel: TextPanelView,
    composition: ResolvedSurfaceComposition,
    theme: Theme,
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
          else (theme.panel.foreground, theme.panel.background)
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
