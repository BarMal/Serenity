package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.config.AppConfig
import com.serenity.state.models.UiSurface
import com.serenity.ui.layout.*
import com.serenity.ui.theme.ColorFormat.withAlpha
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
    val isTabBar     = overlay.surfaceId.contains(UiSurface.TabBarSurfaceId)

    // The status row and the tab strip are both quiet single lines, not floating panels: no shadow, and (via
    // SurfaceFrameLayout) no border.
    if config.surfaceConfig.uiShadowsEnabled && !isStatusLine && !isTabBar then
      surface.roundedRects.foreach(
        _.drawRoundRectShadow(
          rect.x,
          rect.y,
          rect.width,
          rect.height,
          config.scaledUiCornerRadiusPx,
          new Color(0, 0, 0)
        )
      )

    // Scoped to the one surface the status line's colour overrides name -- every other floating panel keeps painting
    // with the theme's panel colours and alpha exactly as before, unmodified (#1295).
    val statusColors = config.statusLine.colors
    val statusBackgroundAlphaOverride: Option[Int] =
      Option
        .when(isStatusLine)(statusColors.backgroundAlpha)
        .flatten
        .map(alpha => math.round(alpha * 255.0).toInt.max(0).min(255))
    val statusForegroundOverride: Option[Color] = Option.when(isStatusLine)(statusColors.foreground).flatten
    val statusBackgroundOverride: Option[Color] = Option.when(isStatusLine)(statusColors.background).flatten

    def rowColors(rowOffset: Int): (Color, Color) =
      val (defaultFg, defaultBg) = overlay.animationState
        .getCell(0, rowOffset)
        .map(cell =>
          (
            cell.currentForeground.getOrElse(theme.panel.foreground),
            cell.currentBackground.getOrElse(theme.panel.background)
          )
        )
        .getOrElse((theme.panel.foreground, theme.panel.background))
      val fg = statusForegroundOverride.getOrElse(defaultFg)
      val bg = statusBackgroundOverride.getOrElse(defaultBg)
      (fg, statusBackgroundAlphaOverride.fold(bg)(bg.withAlpha))

    surface.effects.foreach(_.setAlpha(SurfaceMaterials.panelAlpha(config, theme) * overlay.alphaMultiplier))

    withOptionalRoundRectClip(surface, rect.x, rect.y, rect.width, rect.height, config.scaledUiCornerRadiusPx) {
      for (y, rowOffset) <- (rect.y until rect.bottom).zipWithIndex do
        val (fg, bg) = rowColors(rowOffset)
        surface.setForegroundColor(fg)
        surface.setBackgroundColor(bg)
        surface.putString(rect.x, y, " " * rect.width)

      applyGlassSheen(surface, overlay, theme, config)
      val textInsetPx = overlayTextInsetPx(config)
      overlay.composition match
        case Some(composition) =>
          drawComposition(
            surface,
            composition,
            theme,
            cursorVisible,
            rowColors,
            font,
            cellMetrics,
            textInsetPx,
            overlay.rect.y
          )
        case None =>
          drawContent(surface, overlay, theme, cursorVisible, rowColors, font, cellMetrics, textInsetPx)
    }
    drawBorder(surface, overlay, theme, config)

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
    if rect.width >= 2 && rect.height >= 2 then
      surface.roundedRects.foreach(
        _.strokeRoundRect(
          rect.x,
          rect.y,
          rect.width,
          rect.height,
          config.scaledUiCornerRadiusPx,
          theme.border,
          config.scaledUiOutlineThicknessPx
        )
      )

  /** Falls back to running `render` unclipped when the surface doesn't support rounded-rect clipping -- content still
    * draws, just without the corner mask.
    */
  private def withOptionalRoundRectClip(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int
  )(render: => Unit): Unit =
    surface.roundedRects match
      case Some(rounded) => rounded.withRoundRectClip(x, y, width, height, arcPx)(render)
      case None          => render

  /** How far a surface's text sits inside the content rect the frame laid out for it, beyond the whole cell
    * [[SurfaceFrameLayout]] already reserves for the border. A cell is the smallest inset a frame can express and it is
    * narrower than it is tall, so one cell alone leaves glyphs closer to the border horizontally than vertically, and
    * closest of all to the rounded corner the border is drawn with.
    *
    * Deliberately smaller than a cell: the row's text was measured and truncated against the full content width, so an
    * inset at or beyond a cell would push a full-width row's last glyph past the margin the border sits in.
    */
  private def overlayTextInsetPx(config: AppConfig): Double =
    SpacingScale
      .forUi(config.editorConfig.fontConfig.scaledUiFontSize.toDouble, config.interfaceDensity)
      .px(SpacingStep.Sm)
      .toDouble

  private def drawContent(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    cursorVisible: Boolean,
    rowColors: Int => (Color, Color),
    font: java.awt.Font,
    cellMetrics: CellMetrics,
    textInsetPx: Double
  ): Unit =
    val contentRect = overlay.resolvedContentRect
    val maxLineSize = contentRect.width
    val floatingGeometry = FloatingSurfaceGeometry.fromCells(
      overlay.rect,
      cellMetrics,
      overlay.borderCells,
      overlay.rows.length,
      overlay.header.nonEmpty,
      overlay.footer.nonEmpty,
      overlay.itemGapRows,
      overlay.itemTargetRows,
      overlay.keyHintRow.nonEmpty
    )

    overlay.contentRowSlots
      .foreach { slot =>
        val row = slot.kind match
          case SurfaceContentRowKind.Header      => overlay.header
          case SurfaceContentRowKind.Item(index) => overlay.rows.lift(index)
          case SurfaceContentRowKind.KeyHint     => overlay.keyHintRow
          case SurfaceContentRowKind.Footer      => overlay.footer
        row.foreach { row =>
          val rowOffset        = slot.y - overlay.rect.y
          val (animFg, animBg) = rowColors(rowOffset)
          slot.kind match
            case SurfaceContentRowKind.Item(index) if overlay.itemGapRows > 0.0 =>
              floatingGeometry.itemRects.lift(index).foreach { pixelRect =>
                OverlayRowPainter.renderRow(
                  surface,
                  contentRect.x,
                  slot.y,
                  maxLineSize,
                  row,
                  theme,
                  cursorVisible,
                  defaultForeground = Some(animFg),
                  defaultBackground = Some(animBg),
                  font = font,
                  cellMetrics = cellMetrics,
                  textInsetPx = textInsetPx,
                  pixelY = Some(math.round(pixelRect.y).toInt),
                  pixelHeight = Some(math.round(pixelRect.height).toInt)
                )
              }
            case _ =>
              OverlayRowPainter.renderRow(
                surface,
                contentRect.x,
                slot.y,
                maxLineSize,
                row,
                theme,
                cursorVisible,
                defaultForeground = Some(animFg),
                defaultBackground = Some(animBg),
                font = font,
                cellMetrics = cellMetrics,
                textInsetPx = textInsetPx
              )
        }
      }

  private def drawComposition(
    surface: RenderSurface,
    composition: ResolvedSurfaceComposition,
    theme: Theme,
    cursorVisible: Boolean,
    rowColors: Int => (Color, Color),
    font: Font,
    cellMetrics: CellMetrics,
    textInsetPx: Double,
    frameY: Int
  ): Unit =
    composition.paintBoxes.foreach { box =>
      box.text.foreach { text =>
        val rect      = box.rect
        val x         = math.round(rect.x).toInt
        val y         = math.round(rect.y).toInt
        val width     = math.round(rect.width).toInt
        val rowOffset = y - frameY
        val (fg, bg)  = rowColors(rowOffset)
        val row = OverlayRow(
          plainText = text,
          selected = box.selected,
          cursorColumn = box.cursorOffset,
          segments = box.segments,
          layout = box.layout match
            case SurfacePaintLayout.Plain       => OverlayRowLayout.Plain
            case SurfacePaintLayout.Split       => OverlayRowLayout.Split
            case SurfacePaintLayout.Inline      => OverlayRowLayout.Plain
            case SurfacePaintLayout.Columns     => OverlayRowLayout.Columns
            case SurfacePaintLayout.Distributed => OverlayRowLayout.Distributed
        )
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
          pixelY = Some(cellMetrics.toPixelY(y)),
          pixelHeight = Some(math.max(1, math.round(rect.height).toInt) * cellMetrics.lineHeight)
        )
      }
    }

  private def applyGlassSheen(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    config: AppConfig
  ): Unit =
    SurfaceMaterials.glassSheenBackground(config, theme).foreach { sheenColor =>
      val contentRect = overlay.resolvedContentRect
      val sheenWidth  = contentRect.width
      val sheenHeight = math.min(1, contentRect.height)
      if sheenWidth > 0 && sheenHeight > 0 then
        surface.setBackgroundColor(sheenColor)
        (0 until sheenHeight).foreach { rowOffset =>
          CharacterRenderer.renderStringPlain(surface, contentRect.x, contentRect.y + rowOffset, " " * sheenWidth)
        }
    }
