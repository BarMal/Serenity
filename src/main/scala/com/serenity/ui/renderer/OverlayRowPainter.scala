package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.ui.layout.*
import com.serenity.ui.theme.ColorFormat.withAlpha
import com.serenity.ui.theme.Theme

/** Paints one row of a floating surface: the row's background, then its glyphs in whatever arrangement its
  * [[OverlayRowLayout]] calls for, then its caret. Split out of [[TextOverlayRenderer]], which owns the surface as a
  * whole -- its frame, border, material and content resolution -- and delegates each row here. Sits alongside
  * [[OverlaySegmentRowRenderer]] and [[OverlayColumnRowRenderer]], which it dispatches to for the multi-segment row
  * shapes.
  */
private[renderer] object OverlayRowPainter:

  def renderRow(
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
    cellMetrics: CellMetrics,
    textInsetPx: Double,
    pixelY: Option[Int] = None,
    pixelHeight: Option[Int] = None
  ): Unit =
    surface.text.withLogicalPixelRow(y, pixelY.getOrElse(cellMetrics.toPixelY(y))) {
      renderRowAt(
        surface,
        x,
        y,
        width,
        row,
        theme,
        cursorVisible,
        defaultForeground,
        defaultBackground,
        font,
        cellMetrics,
        textInsetPx,
        pixelY,
        pixelHeight
      )
    }

  private def renderRowAt(
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
    cellMetrics: CellMetrics,
    textInsetPx: Double,
    pixelY: Option[Int],
    pixelHeight: Option[Int]
  ): Unit =
    val rowView = scrolledRowView(row, width)
    val baseFg  = defaultForeground.getOrElse(theme.panel.foreground)
    val baseBg  = defaultBackground.getOrElse(theme.panel.background)
    val rowBackground =
      rowView.row.backgroundColor
        .map(_.withAlpha(baseBg.getAlpha))
        .getOrElse(if rowView.row.selected then theme.highlighted.background.withAlpha(baseBg.getAlpha) else baseBg)
    val rowForeground =
      rowView.row.foregroundColor
        .map(_.withAlpha(baseFg.getAlpha))
        .getOrElse(if rowView.row.selected then theme.highlighted.foreground.withAlpha(baseFg.getAlpha) else baseFg)
    val rowLeftXPx  = cellMetrics.toPixelX(x)
    val rowRightXPx = cellMetrics.toPixelX(x + width)

    surface.setForegroundColor(rowForeground)
    surface.setBackgroundColor(rowBackground)
    if rowView.row.selected then
      pixelHeight.foreach { height =>
        surface.pixels.fillPixelRect(
          xPx = rowLeftXPx,
          yPx = pixelY.getOrElse(cellMetrics.toPixelY(y)),
          widthPx = rowRightXPx - rowLeftXPx,
          heightPx = height,
          color = rowBackground
        )
      }
    if rowView.row.selected then surface.enableStyle(theme.focusStyle)
    CharacterRenderer.renderStringPlain(surface, x, y, " " * width)

    // Only the glyphs move. The row's background fill and its blanking pass above stay full-bleed, so a selected row
    // still spans the whole content rect and the row remains one uninterrupted click target -- the inset is about
    // where text sits inside the row, not about making the row smaller. A terminal's `withPixelTranslation` is the
    // identity, so this is structurally a no-op there rather than a rounding that happens to vanish.
    surface.pixels.withPixelTranslation(textInsetPx, 0.0) {
      renderRowGlyphs(
        surface,
        x,
        y,
        width,
        rowView,
        theme,
        cursorVisible,
        rowForeground,
        rowBackground,
        font,
        cellMetrics,
        pixelY
      )
    }

  private def renderRowGlyphs(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    rowView: OverlayRowView,
    theme: Theme,
    cursorVisible: Boolean,
    rowForeground: Color,
    rowBackground: Color,
    font: java.awt.Font,
    cellMetrics: CellMetrics,
    pixelY: Option[Int]
  ): Unit =
    val rowLeftXPx  = cellMetrics.toPixelX(x)
    val rowRightXPx = cellMetrics.toPixelX(x + width)

    rowView.row.layout match
      case OverlayRowLayout.Plain =>
        // `leadingPadding` indents a Plain row (e.g. the settings-surface group-preview rows) by shrinking its
        // available width and shifting its start column, rather than baking literal spaces into `plainText` --
        // consistent with how `renderCompactDistributedRow` already honors it for the toolbar's row shape.
        val pad           = rowView.row.leadingPadding.max(0).min(width)
        val indentedX     = x + pad
        val indentedWidth = width - pad
        if rowView.row.segments.nonEmpty then
          OverlaySegmentRowRenderer.renderInlineSegments(
            surface,
            indentedX,
            y,
            indentedWidth,
            rowView.row,
            theme,
            rowForeground,
            rowBackground,
            font
          )
        else if rowView.useMeasuredCursor && shouldUseMeasuredCursor(surface) then
          renderMeasuredPlainRow(
            surface,
            indentedX,
            pixelY.getOrElse(cellMetrics.toPixelY(y)),
            indentedWidth,
            rowView.row.plainText,
            font,
            cellMetrics,
            rowRightXPx
          )
        else CharacterRenderer.renderStringPlain(surface, indentedX, y, rowView.row.plainText.take(indentedWidth))
      case OverlayRowLayout.Distributed =>
        OverlaySegmentRowRenderer.renderDistributedRow(
          surface,
          x,
          y,
          width,
          rowView.row,
          theme,
          rowForeground,
          rowBackground,
          font
        )
      case OverlayRowLayout.Split =>
        OverlaySegmentRowRenderer.renderSplitRow(
          surface,
          x,
          y,
          width,
          rowView.row,
          theme,
          rowForeground,
          rowBackground,
          font
        )
      case OverlayRowLayout.Columns =>
        OverlayColumnRowRenderer.renderColumnRow(
          surface,
          x,
          y,
          width,
          rowView.row,
          theme,
          rowForeground,
          rowBackground,
          font
        )
      case OverlayRowLayout.PriorityColumns =>
        OverlayColumnRowRenderer.renderPriorityColumnRow(
          surface,
          x,
          y,
          width,
          rowView.row,
          theme,
          rowForeground,
          rowBackground,
          font
        )

    if cursorVisible then
      rowView.row.cursorColumn
        .flatMap(cursorColumn => cursorPlacement(rowView.row, x, width, cursorColumn, rowView.useMeasuredCursor))
        .foreach { placement =>
          if placement.useMeasured && shouldUseMeasuredCursor(surface) then
            renderMeasuredCursor(
              surface,
              placement.x,
              pixelY.getOrElse(cellMetrics.toPixelY(y)),
              placement.textBeforeCursor,
              theme,
              font,
              cellMetrics,
              rowLeftXPx,
              rowRightXPx
            )
          else if placement.cellColumn >= 0 && placement.cellColumn < width then
            surface.setForegroundColor(theme.background)
            surface.setBackgroundColor(theme.cursor)
            CharacterRenderer.renderChar(surface, placement.x + placement.cellColumn, y, ' ')
        }

    if rowView.row.selected then surface.disableStyle(theme.focusStyle)

  final private case class OverlayRowView(row: OverlayRow, useMeasuredCursor: Boolean)

  final private case class CursorPlacement(x: Int, textBeforeCursor: String, useMeasured: Boolean = false):
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
      case OverlayRowLayout.Columns | OverlayRowLayout.PriorityColumns =>
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
        val (labelWidth, hintWidth, valueWidth) = OverlayColumnRowRenderer.threeColumnWidths(width)
        val valueText                           = OverlayColumnRowRenderer.fitCellText(value.text, valueWidth)
        val valueX = x + labelWidth + hintWidth + 2 + math.max(0, valueWidth - valueText.length)
        Some(CursorPlacement(valueX, valueText, useMeasured = true))
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
        case OverlayRowLayout.Columns | OverlayRowLayout.PriorityColumns | OverlayRowLayout.Distributed =>
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

  // #1105: drawRunPx is a no-op on a surface with no FontRenderContext (a terminal), so the measured path can never be
  // taken there regardless of what the font alone would call for (ligatures, proportional advances, ...). Every real
  // (GUI) surface reports a FontRenderContext unconditionally, so this was already the de facto behaviour for GUI mode
  // -- the font-only checks previously OR'd in here never had the chance to matter on a real surface, only on a
  // cell-only one, where they were exactly the bug: they could force the measured (dropped) path even with no
  // FontRenderContext to measure against.
  private def shouldUseMeasuredCursor(surface: RenderSurface): Boolean =
    surface.text.fontRenderContext.nonEmpty

  private def renderMeasuredPlainRow(
    surface: RenderSurface,
    x: Int,
    yPx: Int,
    width: Int,
    text: String,
    font: java.awt.Font,
    cellMetrics: CellMetrics,
    maxRightXPx: Int
  ): Unit =
    val visibleText = text.take(width)
    if visibleText.nonEmpty then
      val frc        = surface.text.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
      val caretXs    = TextLayoutSnapshot.caretXsForText(visibleText, font, frc)
      val textXPx    = cellMetrics.toPixelX(x).toFloat
      val maxWidthPx = math.max(1.0f, maxRightXPx.toFloat - textXPx)
      val widthPx    = caretXs.lastOption.getOrElse(0.0f).max(1.0f).min(maxWidthPx)
      surface.text.drawRunPx(textXPx, yPx, widthPx, cellMetrics.lineHeight, cellMetrics.ascent, visibleText)

  private def renderMeasuredCursor(
    surface: RenderSurface,
    x: Int,
    yPx: Int,
    textBeforeCursor: String,
    theme: Theme,
    font: java.awt.Font,
    cellMetrics: CellMetrics,
    minXPx: Int,
    maxRightXPx: Int
  ): Unit =
    val frc          = surface.text.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    val caretXs      = TextLayoutSnapshot.caretXsForText(textBeforeCursor, font, frc)
    val rawWidthPx   = math.max(2, math.round(cellMetrics.charWidth * 0.12f))
    val caretWidthPx = math.min(rawWidthPx, math.max(1, maxRightXPx - minXPx))
    val unclampedXPx = cellMetrics.toPixelX(x) + math.round(caretXs.lastOption.getOrElse(0.0f))
    val xPx          = math.max(minXPx, math.min(unclampedXPx, maxRightXPx - caretWidthPx))
    surface.pixels.fillPixelRect(xPx, yPx, caretWidthPx, cellMetrics.lineHeight, theme.cursor)
