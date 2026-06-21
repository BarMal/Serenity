package com.serenity.ui.renderer

import java.awt.{Color, Font}

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
    drawBorder(surface, overlay, theme, config)
    drawContent(surface, overlay, theme, cursorVisible, rowColors, animated, font, cellMetrics)

    surface.setAlpha(1.0f)
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
      surface.strokeRoundRect(rect.x, rect.y, rect.width, rect.height, config.uiCornerRadiusPx, theme.border)

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
          defaultForeground = Some(animFg),
          defaultBackground = Some(animBg),
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
    defaultForeground: Option[Color] = None,
    defaultBackground: Option[Color] = None,
    isAnimating: Boolean = false,
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val rowView = scrolledRowView(row, width)
    val baseFg  = defaultForeground.getOrElse(theme.panel.foreground)
    val baseBg  = defaultBackground.getOrElse(theme.panel.background)
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
        renderDistributedRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font, isAnimating)
      case OverlayRowLayout.Split =>
        renderSplitRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font, isAnimating)
      case OverlayRowLayout.Columns =>
        renderColumnRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font, isAnimating)

    if cursorVisible then
      rowView.row.cursorColumn
        .flatMap(cursorColumn => cursorPlacement(rowView.row, x, width, cursorColumn))
        .foreach { placement =>
          if shouldUseMeasuredCursor(font, surface) then
            renderMeasuredCursor(surface, placement.x, y, placement.textBeforeCursor, theme, font, cellMetrics)
          else if placement.cellColumn >= 0 && placement.cellColumn < width then
            surface.setForegroundColor(theme.background)
            surface.setBackgroundColor(theme.cursor)
            CharacterRenderer.renderChar(surface, placement.x + placement.cellColumn, y, ' ')
        }

  private case class OverlayRowView(row: OverlayRow)

  private case class CursorPlacement(x: Int, textBeforeCursor: String):
    def cellColumn: Int =
      textBeforeCursor.length

  private def cursorPlacement(row: OverlayRow, x: Int, width: Int, cursorColumn: Int): Option[CursorPlacement] =
    row.layout match
      case OverlayRowLayout.Plain =>
        Some(CursorPlacement(x, row.plainText.take(cursorColumn.max(0).min(row.plainText.length))))
      case OverlayRowLayout.Split =>
        splitCursorPlacement(row, x, width, cursorColumn)
      case OverlayRowLayout.Columns =>
        columnCursorPlacement(row, x, width)
      case OverlayRowLayout.Distributed =>
        None

  private def splitCursorPlacement(
    row: OverlayRow,
    x: Int,
    width: Int,
    cursorColumn: Int
  ): Option[CursorPlacement] =
    row.segments match
      case left :: rightSegments if rightSegments.nonEmpty =>
        val rightTexts      = rightSegments.map(_.text)
        val rightGroupText  = rightTexts.mkString(" ")
        val rightGroupWidth = math.min(width, rightGroupText.length)
        val rightStartX     = x + math.max(0, width - rightGroupWidth)
        val rightStartCol   = left.text.length + 1
        if cursorColumn <= left.text.length then
          Some(CursorPlacement(x, left.text.take(cursorColumn.max(0).min(left.text.length))))
        else
          val localColumn = (cursorColumn - rightStartCol).max(0).min(rightGroupText.length)
          Some(CursorPlacement(rightStartX, rightGroupText.take(localColumn)))
      case _ =>
        Some(CursorPlacement(x, row.plainText.take(cursorColumn.max(0).min(row.plainText.length))))

  private def columnCursorPlacement(row: OverlayRow, x: Int, width: Int): Option[CursorPlacement] =
    row.segments match
      case _ :: _ :: value :: Nil if value.selected =>
        val labelWidth = math.min(22, math.max(8, width / 3))
        val valueWidth = math.min(18, math.max(8, width / 4))
        val hintWidth  = math.max(0, width - labelWidth - valueWidth - 2)
        Some(CursorPlacement(x + labelWidth + hintWidth + 2, value.text.take(valueWidth)))
      case _ =>
        row.cursorColumn.map(cursorColumn =>
          CursorPlacement(x, row.plainText.take(cursorColumn.max(0).min(row.plainText.length)))
        )

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
    font: Font,
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
            font,
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
    font: Font,
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
          font,
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
            font,
            isAnimating
          )
          cursorX + text.length + 1
        }
      case _ =>
        CharacterRenderer.renderStringPlain(surface, x, y, row.plainText.take(width))

  private def renderColumnRow(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    row: OverlayRow,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    font: Font,
    isAnimating: Boolean = false
  ): Unit =
    row.segments match
      case label :: hint :: value :: Nil =>
        val labelWidth = math.min(22, math.max(8, width / 3))
        val valueWidth = math.min(18, math.max(8, width / 4))
        val hintWidth  = math.max(0, width - labelWidth - valueWidth - 2)
        renderColumnCell(
          surface,
          x,
          y,
          labelWidth,
          label,
          row.selected,
          theme,
          defaultForeground,
          defaultBackground,
          font
        )
        renderColumnCell(
          surface,
          x + labelWidth + 1,
          y,
          hintWidth,
          hint,
          row.selected,
          theme,
          defaultForeground,
          defaultBackground,
          font
        )
        renderColumnCell(
          surface,
          x + labelWidth + hintWidth + 2,
          y,
          valueWidth,
          value,
          row.selected,
          theme,
          defaultForeground,
          defaultBackground,
          font
        )
      case label :: hint :: Nil =>
        val labelWidth = math.min(22, math.max(8, width / 3))
        val hintWidth  = math.max(0, width - labelWidth - 1)
        renderColumnCell(
          surface,
          x,
          y,
          labelWidth,
          label,
          row.selected,
          theme,
          defaultForeground,
          defaultBackground,
          font
        )
        renderColumnCell(
          surface,
          x + labelWidth + 1,
          y,
          hintWidth,
          hint,
          row.selected,
          theme,
          defaultForeground,
          defaultBackground,
          font
        )
      case _ =>
        CharacterRenderer.renderStringPlain(surface, x, y, row.plainText.take(width))

  private def renderColumnCell(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    segment: OverlaySegment,
    rowSelected: Boolean,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    font: Font
  ): Unit =
    val text =
      if rowSelected && segment.text.length > width then segment.text.takeRight(width)
      else segment.text.take(width)
    renderSegmentText(
      surface,
      x,
      y,
      width,
      text,
      segment,
      theme,
      defaultForeground,
      defaultBackground,
      font = font
    )

  private def renderSegmentCell(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    segment: OverlaySegment,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    font: Font,
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
      font,
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
    font: Font,
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
      segment.fontFamily.foreach(family =>
        surface.setFont(Font(family, font.getStyle, font.getSize).deriveFont(font.getSize2D))
      )
      CharacterRenderer.renderStringPlain(surface, x, y, segmentText.take(width))
      if segment.fontFamily.nonEmpty then surface.setFont(font)

  private def shouldUseMeasuredCursor(font: java.awt.Font, surface: RenderSurface): Boolean =
    FontLoader.ligaturesEnabled(font) || !FontLoader.isMonospacedFont(font) || surface.fontRenderContext.nonEmpty

  private def renderMeasuredCursor(
    surface: RenderSurface,
    x: Int,
    y: Int,
    textBeforeCursor: String,
    theme: Theme,
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val frc          = surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    val caretXs      = TextLayoutSnapshot.caretXsForText(textBeforeCursor, font, frc)
    val xPx          = cellMetrics.toPixelX(x) + math.round(caretXs.lastOption.getOrElse(0.0f))
    val yPx          = cellMetrics.toPixelY(y)
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
