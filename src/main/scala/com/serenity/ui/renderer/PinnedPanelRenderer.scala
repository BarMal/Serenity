package com.serenity.ui.renderer

import com.serenity.animation.AnimationState
import com.serenity.config.AppConfig
import com.serenity.ui.layout.SurfaceContentRowKind
import com.serenity.ui.theme.Theme

object PinnedPanelRenderer:

  private val BorderAnimationColumn = -1
  private val BorderAnimationRow    = -1

  def render(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    config: AppConfig,
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
          config.uiCornerRadiusPx,
          new java.awt.Color(0, 0, 0)
        )
      )
    surface.effects.foreach(_.setAlpha(SurfaceMaterials.panelAlpha(config, theme)))
    surface.setForegroundColor(theme.panel.foreground)
    surface.setBackgroundColor(theme.panel.background)

    for y <- rect.y until rect.bottom do surface.putString(rect.x, y, " " * rect.width)

    applyGlassSheen(surface, panel, theme, config)
    drawBorder(surface, panel, theme, config, animationState)
    drawTitle(surface, panel, theme, animationState)
    drawLines(surface, panel, theme, animationState)

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
          config.uiCornerRadiusPx,
          borderColor,
          config.uiOutlineThicknessPx.toFloat
        )
      )

  private def drawTitle(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    animationState: AnimationState
  ): Unit =
    val titleRect = panel.titleRect
    val title     = panel.title.take(titleRect.width).padTo(titleRect.width, ' ')
    if titleRect.width > 0 then
      renderAnimatedText(surface, titleRect.x, titleRect.y, title, 0, theme.panel.foreground, animationState)

  private def drawLines(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme,
    animationState: AnimationState
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
          if row.selected then
            surface.setForegroundColor(theme.highlighted.foreground)
            surface.setBackgroundColor(theme.highlighted.background)
            surface.enableStyle(theme.focusStyle)
          else
            surface.setForegroundColor(theme.panel.foreground)
            surface.setBackgroundColor(theme.panel.background)
          val baseForeground =
            if row.selected then theme.highlighted.foreground else theme.panel.foreground
          renderAnimatedText(
            surface,
            contentRect.x,
            slot.y,
            padded,
            slot.y - panel.rect.y,
            baseForeground,
            animationState
          )
          if row.selected then surface.disableStyle(theme.focusStyle)
        }
      }

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
