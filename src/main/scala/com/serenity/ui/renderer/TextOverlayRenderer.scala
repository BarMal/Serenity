package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.config.AppConfig
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, SurfaceFrameLayout, TextLayoutSnapshot}
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

    drawBorder(surface, overlay, theme, config)
    drawContent(surface, overlay, theme, cursorVisible, rowColors, font, cellMetrics)

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
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val frameLayout = SurfaceFrameLayout(overlay.rect)
    val contentRect = frameLayout.contentRect
    val maxLineSize = contentRect.width
    val maxLines    = frameLayout.maxContentRows

    val contentRows = overlay.header.toList ++ overlay.rows ++ overlay.footer.toList

    contentRows.take(maxLines).zipWithIndex.foreach {
      case (row, index) =>
        val rowOffset        = 1 + index
        val (animFg, animBg) = rowColors(rowOffset)
        renderRow(
          surface,
          contentRect.x,
          contentRect.y + index,
          maxLineSize,
          row,
          theme,
          cursorVisible,
          defaultForeground = Some(animFg),
          defaultBackground = Some(animBg),
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
    defaultForeground: Option[Color],
    defaultBackground: Option[Color],
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val rowView = scrolledRowView(row, width)
    val baseFg  = defaultForeground.getOrElse(theme.panel.foreground)
    val baseBg  = defaultBackground.getOrElse(theme.panel.background)
    val rowBackground =
      rowView.row.backgroundColor
        .map(color => withAlpha(color, baseBg.getAlpha))
        .getOrElse(if rowView.row.selected then withAlpha(theme.highlighted.background, baseBg.getAlpha) else baseBg)
    val rowForeground =
      rowView.row.foregroundColor
        .map(color => withAlpha(color, baseFg.getAlpha))
        .getOrElse(if rowView.row.selected then withAlpha(theme.highlighted.foreground, baseFg.getAlpha) else baseFg)

    surface.setForegroundColor(rowForeground)
    surface.setBackgroundColor(rowBackground)
    CharacterRenderer.renderStringPlain(surface, x, y, " " * width)

    rowView.row.layout match
      case OverlayRowLayout.Plain =>
        if rowView.useMeasuredCursor && shouldUseMeasuredCursor(font, surface) then
          renderMeasuredPlainRow(surface, x, y, width, rowView.row.plainText, font, cellMetrics)
        else CharacterRenderer.renderStringPlain(surface, x, y, rowView.row.plainText.take(width))
      case OverlayRowLayout.Distributed =>
        renderDistributedRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font)
      case OverlayRowLayout.Split =>
        renderSplitRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font)
      case OverlayRowLayout.Columns =>
        renderColumnRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font)

    if cursorVisible then
      rowView.row.cursorColumn
        .flatMap(cursorColumn => cursorPlacement(rowView.row, x, width, cursorColumn, rowView.useMeasuredCursor))
        .foreach { placement =>
          if placement.useMeasured && shouldUseMeasuredCursor(font, surface) then
            renderMeasuredCursor(surface, placement.x, y, placement.textBeforeCursor, theme, font, cellMetrics)
          else if placement.cellColumn >= 0 && placement.cellColumn < width then
            surface.setForegroundColor(theme.background)
            surface.setBackgroundColor(theme.cursor)
            CharacterRenderer.renderChar(surface, placement.x + placement.cellColumn, y, ' ')
        }

  private case class OverlayRowView(row: OverlayRow, useMeasuredCursor: Boolean)

  private case class CursorPlacement(x: Int, textBeforeCursor: String, useMeasured: Boolean = false):
    def cellColumn: Int =
      textBeforeCursor.length

  private def cursorPlacement(
    row: OverlayRow,
    x: Int,
    width: Int,
    cursorColumn: Int,
    useMeasuredCursor: Boolean
  ): Option[CursorPlacement] =
    row.layout match
      case OverlayRowLayout.Plain =>
        Some(CursorPlacement(x, row.plainText.take(cursorColumn.max(0).min(row.plainText.length)), useMeasuredCursor))
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
        val rightTexts     = rightSegments.map(_.text)
        val rightGroupText = rightTexts.mkString(" ")
        val rightStartCol  = left.text.length + 1
        val rightStartX =
          if row.cursorColumn.nonEmpty then x + rightStartCol
          else
            val rightGroupWidth = math.min(width, rightGroupText.length)
            x + math.max(0, width - rightGroupWidth)
        if cursorColumn <= left.text.length then
          Some(CursorPlacement(x, left.text.take(cursorColumn.max(0).min(left.text.length)), useMeasured = true))
        else
          val localColumn = (cursorColumn - rightStartCol).max(0).min(rightGroupText.length)
          Some(CursorPlacement(rightStartX, rightGroupText.take(localColumn), useMeasured = true))
      case _ =>
        Some(CursorPlacement(x, row.plainText.take(cursorColumn.max(0).min(row.plainText.length))))

  private def columnCursorPlacement(row: OverlayRow, x: Int, width: Int): Option[CursorPlacement] =
    row.segments match
      case _ :: _ :: value :: Nil if value.selected =>
        val labelWidth = math.min(22, math.max(8, width / 3))
        val valueWidth = math.min(18, math.max(8, width / 4))
        val hintWidth  = math.max(0, width - labelWidth - valueWidth - 2)
        Some(CursorPlacement(x + labelWidth + hintWidth + 2, fitCellText(value.text, valueWidth), useMeasured = true))
      case _ =>
        row.cursorColumn.map(cursorColumn =>
          CursorPlacement(x, row.plainText.take(cursorColumn.max(0).min(row.plainText.length)))
        )

  private def scrolledRowView(row: OverlayRow, width: Int): OverlayRowView =
    val useMeasuredCursor = row.cursorColumn.nonEmpty
    val scrollOffset =
      row.layout match
        case OverlayRowLayout.Plain | OverlayRowLayout.Split =>
          row.cursorColumn match
            case Some(cursorColumn) if row.plainText.length > width =>
              math.max(0, math.min(cursorColumn - width + 1, row.plainText.length - width))
            case _ =>
              0
        case OverlayRowLayout.Columns | OverlayRowLayout.Distributed =>
          0

    if scrollOffset == 0 then OverlayRowView(row, useMeasuredCursor)
    else
      val visibleText = row.plainText.slice(scrollOffset, scrollOffset + width)
      OverlayRowView(
        row.copy(
          plainText = visibleText,
          cursorColumn = row.cursorColumn.map(_ - scrollOffset).filter(_ >= 0),
          segments = Nil,
          layout = OverlayRowLayout.Plain
        ),
        useMeasuredCursor
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
    font: Font
  ): Unit =
    val segments = row.segments
    if segments.isEmpty then CharacterRenderer.renderStringPlain(surface, x, y, row.plainText.take(width))
    else
      val baseCellWidth = width / segments.length
      val remainder     = width % segments.length

      val _ = segments.zipWithIndex.foldLeft(x) {
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
            font
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
    font: Font
  ): Unit =
    row.segments match
      case left :: rightSegments if rightSegments.nonEmpty =>
        if row.cursorColumn.nonEmpty then
          renderEditableSplitRow(
            surface,
            x,
            y,
            width,
            left,
            rightSegments,
            theme,
            defaultForeground,
            defaultBackground,
            font
          )
        else
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
            font
          )

          val rightStartX = x + math.max(0, width - rightGroupWidth)
          val _ = rightSegments.foldLeft(rightStartX) { (cursorX, segment) =>
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
              font
            )
            cursorX + text.length + 1
          }
      case _ =>
        CharacterRenderer.renderStringPlain(surface, x, y, row.plainText.take(width))

  private def renderEditableSplitRow(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    left: OverlaySegment,
    rightSegments: List[OverlaySegment],
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    font: Font
  ): Unit =
    val leftText = left.text.take(width)
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
      font
    )

    val firstRightX = x + leftText.length + 1
    val _ = rightSegments.foldLeft(firstRightX) { (cursorX, segment) =>
      val remainingWidth = math.max(0, x + width - cursorX)
      val text           = segment.text.take(remainingWidth)
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
        font
      )
      cursorX + text.length + 1
    }

  private def renderColumnRow(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    row: OverlayRow,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    font: Font
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
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    font: Font
  ): Unit =
    val text = fitCellText(segment.text, width)
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

  private def fitCellText(text: String, width: Int): String =
    if width <= 0 then ""
    else if text.length <= width then text
    else if width <= 3 then text.take(width)
    else text.take(width - 3) + "..."

  private def renderSegmentCell(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    segment: OverlaySegment,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    font: Font
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
      font
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
    font: Font
  ): Unit =
    if width > 0 then
      val segmentBackground =
        segment.backgroundColor
          .map(color => withAlpha(color, defaultBackground.getAlpha))
          .getOrElse(
            if segment.selected then withAlpha(theme.highlighted.background, defaultBackground.getAlpha)
            else if segment.tone == OverlayTone.Error then withAlpha(theme.error.background, defaultBackground.getAlpha)
            else defaultBackground
          )
      val segmentForeground =
        segment.foregroundColor
          .map(color => withAlpha(color, defaultForeground.getAlpha))
          .getOrElse(
            if segment.selected then withAlpha(theme.highlighted.foreground, defaultForeground.getAlpha)
            else if segment.tone == OverlayTone.Muted then withAlpha(theme.muted, defaultForeground.getAlpha)
            else if segment.tone == OverlayTone.Error then withAlpha(theme.error.foreground, defaultForeground.getAlpha)
            else defaultForeground
          )
      surface.setForegroundColor(segmentForeground)
      surface.setBackgroundColor(segmentBackground)
      segment.fontFamily.foreach(family =>
        surface.setFont(Font(family, font.getStyle, font.getSize).deriveFont(font.getSize2D))
      )
      CharacterRenderer.renderStringPlain(surface, x, y, segmentText.take(width))
      if segment.fontFamily.nonEmpty then surface.setFont(font)

  private def withAlpha(color: Color, alpha: Int): Color =
    if color.getAlpha == alpha then color
    else new Color(color.getRed, color.getGreen, color.getBlue, alpha)

  private def shouldUseMeasuredCursor(font: java.awt.Font, surface: RenderSurface): Boolean =
    FontLoader.ligaturesEnabled(font) || !FontLoader.isMonospacedFont(font) || surface.fontRenderContext.nonEmpty

  private def renderMeasuredPlainRow(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    text: String,
    font: java.awt.Font,
    cellMetrics: CellMetrics
  ): Unit =
    val visibleText = text.take(width)
    if visibleText.nonEmpty then
      val frc     = surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
      val caretXs = TextLayoutSnapshot.caretXsForText(visibleText, font, frc)
      val textXPx = cellMetrics.toPixelX(x).toFloat
      val textYPx = cellMetrics.toPixelY(y)
      val widthPx = caretXs.lastOption.getOrElse(0.0f).max(1.0f)
      surface.drawRunPx(textXPx, textYPx, widthPx, cellMetrics.lineHeight, cellMetrics.ascent, visibleText)

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
      val contentRect = SurfaceFrameLayout(rect).contentRect
      val sheenWidth  = contentRect.width
      val sheenHeight = math.min(2, contentRect.height)
      if sheenWidth > 0 && sheenHeight > 0 then
        surface.setBackgroundColor(sheenColor)
        (0 until sheenHeight).foreach { rowOffset =>
          CharacterRenderer.renderStringPlain(surface, contentRect.x, contentRect.y + rowOffset, " " * sheenWidth)
        }
    }
