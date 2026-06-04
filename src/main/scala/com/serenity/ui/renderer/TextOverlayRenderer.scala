package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.config.AppConfig
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
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
    val rect = overlay.rect

    def rowColors(rowOffset: Int): (Color, Color) =
      overlay.animationState
        .getCell(0, rowOffset)
        .map(cell =>
          (
            cell.currentForeground.getOrElse(theme.panel.foreground),
            cell.currentBackground.getOrElse(theme.panel.background)
          )
        )
        .getOrElse((theme.panel.foreground, theme.panel.background))

    surface.setAlpha(SurfaceMaterials.panelAlpha(config, theme) * overlay.alphaMultiplier)

    for (y, rowOffset) <- (rect.y until rect.bottom).zipWithIndex do
      val (fg, bg) = rowColors(rowOffset)
      surface.setForegroundColor(fg)
      surface.setBackgroundColor(bg)
      surface.putString(rect.x, y, " " * rect.width)

    applyGlassSheen(surface, rect, theme, config)

    val animated = overlay.animationState.animations.nonEmpty
    drawBorder(surface, overlay, theme)
    drawContent(surface, overlay, theme, cursorVisible, rowColors, animated, font, cellMetrics)

    surface.setAlpha(1.0f)
    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)

  private def drawBorder(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme
  ): Unit =
    val rect = overlay.rect
    if rect.width >= 2 && rect.height >= 2 then
      surface.strokeRoundRect(rect.x, rect.y, rect.width, rect.height, arcPx = 8, theme.border)

  private def drawContent(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    cursorVisible: Boolean,
    rowColors: Int => (Color, Color),
    animated: Boolean,
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val rect        = overlay.rect
    val maxLineSize = math.max(0, rect.width - 2)
    val maxLines    = math.max(0, rect.height - 2)

    val contentRows = overlay.header.toList ++ overlay.rows ++ overlay.footer.toList

    contentRows.take(maxLines).zipWithIndex.foreach {
      case (row, index) =>
        val rowOffset        = 1 + index
        val (animFg, animBg) = rowColors(rowOffset)
        renderRow(
          surface,
          rect.x + 1,
          rect.y + 1 + index,
          maxLineSize,
          row,
          theme,
          cursorVisible,
          defaultForeground = animFg,
          defaultBackground = animBg,
          isAnimating = animated,
          font = font,
          cellMetrics = cellMetrics
        )
    }

  private def renderRow(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    row: OverlayRow,
    theme: Theme,
    cursorVisible: Boolean,
    defaultForeground: Color = null,
    defaultBackground: Color = null,
    isAnimating: Boolean = false,
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val rowView = scrolledRowView(row, width)
    val baseFg = if defaultForeground != null then defaultForeground else theme.panel.foreground
    val baseBg = if defaultBackground != null then defaultBackground else theme.panel.background
    val rowBackground =
      rowView.row.backgroundColor.getOrElse(
        if rowView.row.selected then theme.highlighted.background else baseBg
      )
    val rowForeground =
      rowView.row.foregroundColor.getOrElse(
        if rowView.row.selected then theme.highlighted.foreground else baseFg
      )

    surface.setForegroundColor(rowForeground)
    surface.setBackgroundColor(rowBackground)
    CharacterRenderer.renderStringPlain(surface, x, y, " " * width)

    rowView.row.layout match
      case OverlayRowLayout.Plain =>
        CharacterRenderer.renderStringPlain(surface, x, y, rowView.row.plainText.take(width))
      case OverlayRowLayout.Distributed =>
        renderDistributedRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, isAnimating)
      case OverlayRowLayout.Split =>
        renderSplitRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, isAnimating)

    if cursorVisible then
      rowView.row.cursorColumn.foreach { cursorColumn =>
        if rowView.row.layout == OverlayRowLayout.Plain && shouldUseMeasuredCursor(font, surface) then
          renderMeasuredCursor(surface, x, y, rowView.row, cursorColumn, theme, font, cellMetrics)
        else if cursorColumn >= 0 && cursorColumn < width then
          val cursorX = x + cursorColumn
          val cursorChar =
            if cursorColumn < rowView.row.plainText.length then rowView.row.plainText.charAt(cursorColumn)
            else ' '
          surface.setForegroundColor(theme.background)
          surface.setBackgroundColor(theme.cursor)
          CharacterRenderer.renderChar(surface, cursorX, y, cursorChar)
      }

  private case class OverlayRowView(row: OverlayRow)

  private def scrolledRowView(row: OverlayRow, width: Int): OverlayRowView =
    val scrollOffset =
      row.cursorColumn match
        case Some(cursorColumn) if row.plainText.length > width =>
          math.max(0, math.min(cursorColumn - width + 1, row.plainText.length - width))
        case _ =>
          0

    if scrollOffset == 0 then OverlayRowView(row)
    else
      val visibleText = row.plainText.slice(scrollOffset, scrollOffset + width)
      OverlayRowView(
        row.copy(
          plainText = visibleText,
          cursorColumn = row.cursorColumn.map(_ - scrollOffset).filter(_ >= 0),
          segments = Nil,
          layout = OverlayRowLayout.Plain
        )
      )

  private def renderDistributedRow(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    row: OverlayRow,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    isAnimating: Boolean = false
  ): Unit =
    val segments = row.segments
    if segments.isEmpty then CharacterRenderer.renderStringPlain(surface, x, y, row.plainText.take(width))
    else
      val baseCellWidth = width / segments.length
      val remainder     = width % segments.length

      segments.zipWithIndex.foldLeft(x) {
        case (cursorX, (segment, index)) =>
          val cellWidth = baseCellWidth + (if index < remainder then 1 else 0)
          renderSegmentCell(
            surface,
            cursorX,
            y,
            cellWidth,
            segment,
            theme,
            defaultForeground,
            defaultBackground,
            isAnimating
          )
          cursorX + cellWidth
      }
      ()

  private def renderSplitRow(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    row: OverlayRow,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    isAnimating: Boolean = false
  ): Unit =
    row.segments match
      case left :: rightSegments if rightSegments.nonEmpty =>
        val rightTexts      = rightSegments.map(_.text)
        val rightGroupText  = rightTexts.mkString(" ")
        val rightGroupWidth = math.min(width, rightGroupText.length)
        val leftMaxWidth    = math.max(0, width - rightGroupWidth - 1)
        val leftText        = left.text.take(leftMaxWidth)

        renderSegmentText(
          surface,
          x,
          y,
          leftText.length,
          leftText,
          left,
          theme,
          defaultForeground,
          defaultBackground,
          isAnimating
        )

        val rightStartX = x + math.max(0, width - rightGroupWidth)
        rightSegments.foldLeft(rightStartX) { (cursorX, segment) =>
          val text = segment.text.take(math.max(0, x + width - cursorX))
          renderSegmentText(
            surface,
            cursorX,
            y,
            text.length,
            text,
            segment,
            theme,
            defaultForeground,
            defaultBackground,
            isAnimating
          )
          cursorX + text.length + 1
        }
      case _ =>
        CharacterRenderer.renderStringPlain(surface, x, y, row.plainText.take(width))

  private def renderSegmentCell(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    segment: OverlaySegment,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    isAnimating: Boolean = false
  ): Unit =
    val text    = segment.text.take(width)
    val leftPad = math.max(0, (width - text.length) / 2)
    val renderX = x + leftPad
    renderSegmentText(
      surface,
      renderX,
      y,
      text.length,
      text,
      segment,
      theme,
      defaultForeground,
      defaultBackground,
      isAnimating
    )

  private def renderSegmentText(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    segmentText: String,
    segment: OverlaySegment,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    isAnimating: Boolean = false
  ): Unit =
    if width > 0 then
      val segmentBackground =
        segment.backgroundColor.getOrElse(
          if segment.selected then theme.highlighted.background
          else if segment.tone == OverlayTone.Error then theme.error.background
          else defaultBackground
        )
      val segmentForeground =
        segment.foregroundColor.getOrElse(
          if segment.selected then theme.highlighted.foreground
          else if segment.tone == OverlayTone.Muted then theme.muted
          else if segment.tone == OverlayTone.Error then theme.error.foreground
          else defaultForeground
        )
      surface.setForegroundColor(segmentForeground)
      surface.setBackgroundColor(segmentBackground)
      CharacterRenderer.renderStringPlain(surface, x, y, segmentText.take(width))

  private def shouldUseMeasuredCursor(font: java.awt.Font, surface: RenderSurface): Boolean =
    FontLoader.ligaturesEnabled(font) || !FontLoader.isMonospacedFont(font) || surface.fontRenderContext.nonEmpty

  private def renderMeasuredCursor(
    surface: RenderSurface,
    x: Int,
    y: Int,
    row: OverlayRow,
    cursorColumn: Int,
    theme: Theme,
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val frc = surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    val caretXs = TextLayoutSnapshot.caretXsForText(row.plainText, font, frc)
    val safeColumn = cursorColumn.max(0).min(caretXs.length - 1)
    val xPx = cellMetrics.toPixelX(x) + math.round(caretXs(safeColumn))
    val yPx = cellMetrics.toPixelY(y)
    val caretWidthPx = math.max(2, math.round(cellMetrics.charWidth * 0.12f))
    surface.fillPixelRect(xPx, yPx, caretWidthPx, cellMetrics.lineHeight, theme.cursor)

  private def applyGlassSheen(
    surface: RenderSurface,
    rect: com.serenity.ui.layout.LayoutRect,
    theme: Theme,
    config: AppConfig
  ): Unit =
    SurfaceMaterials.glassSheenBackground(config, theme).foreach { sheenColor =>
      val sheenWidth  = math.max(0, rect.width - 2)
      val sheenHeight = math.min(2, math.max(0, rect.height - 2))
      if sheenWidth > 0 && sheenHeight > 0 then
        surface.setBackgroundColor(sheenColor)
        (0 until sheenHeight).foreach { rowOffset =>
          CharacterRenderer.renderStringPlain(surface, rect.x + 1, rect.y + 1 + rowOffset, " " * sheenWidth)
        }
    }
