package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.config.AppConfig
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

    if config.surfaceConfig.uiShadowsEnabled then
      surface.roundedRects.foreach(
        _.drawRoundRectShadow(rect.x, rect.y, rect.width, rect.height, config.uiCornerRadiusPx, new Color(0, 0, 0))
      )

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

    surface.effects.foreach(_.setAlpha(SurfaceMaterials.panelAlpha(config, theme) * overlay.alphaMultiplier))

    withOptionalRoundRectClip(surface, rect.x, rect.y, rect.width, rect.height, config.uiCornerRadiusPx) {
      for (y, rowOffset) <- (rect.y until rect.bottom).zipWithIndex do
        val (fg, bg) = rowColors(rowOffset)
        surface.setForegroundColor(fg)
        surface.setBackgroundColor(bg)
        surface.putString(rect.x, y, " " * rect.width)

      applyGlassSheen(surface, overlay, theme, config)
      overlay.composition match
        case Some(composition) =>
          drawComposition(surface, composition, theme, cursorVisible, rowColors, font, cellMetrics, overlay.rect.y)
        case None =>
          drawContent(surface, overlay, theme, cursorVisible, rowColors, font, cellMetrics)
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
          config.uiCornerRadiusPx,
          theme.border,
          config.uiOutlineThicknessPx.toFloat
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

  private def drawContent(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    cursorVisible: Boolean,
    rowColors: Int => (Color, Color),
    font: java.awt.Font,
    cellMetrics: CellMetrics
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
                renderRow(
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
                  pixelY = Some(math.round(pixelRect.y).toInt),
                  pixelHeight = Some(math.round(pixelRect.height).toInt)
                )
              }
            case _ =>
              renderRow(
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
                cellMetrics = cellMetrics
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
            case SurfacePaintLayout.Plain  => OverlayRowLayout.Plain
            case SurfacePaintLayout.Split  => OverlayRowLayout.Split
            case SurfacePaintLayout.Inline => OverlayRowLayout.Plain
        )
        renderRow(
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
          pixelY = Some(cellMetrics.toPixelY(y)),
          pixelHeight = Some(math.max(1, math.round(rect.height).toInt) * cellMetrics.lineHeight)
        )
      }
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
    cellMetrics: CellMetrics,
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

    rowView.row.layout match
      case OverlayRowLayout.Plain =>
        // `leadingPadding` indents a Plain row (e.g. the settings-surface group-preview rows) by shrinking its
        // available width and shifting its start column, rather than baking literal spaces into `plainText` --
        // consistent with how `renderCompactDistributedRow` already honors it for the toolbar's row shape.
        val pad           = rowView.row.leadingPadding.max(0).min(width)
        val indentedX     = x + pad
        val indentedWidth = width - pad
        if rowView.row.segments.nonEmpty then
          renderInlineSegments(
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
        renderDistributedRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font)
      case OverlayRowLayout.Split =>
        renderSplitRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font)
      case OverlayRowLayout.Columns =>
        renderColumnRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font)
      case OverlayRowLayout.PriorityColumns =>
        renderPriorityColumnRow(surface, x, y, width, rowView.row, theme, rowForeground, rowBackground, font)

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
        val (labelWidth, hintWidth, valueWidth) = threeColumnWidths(width)
        val valueText                           = fitCellText(value.text, valueWidth)
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
    else if segments.exists(_.allocatedWidth.nonEmpty) then
      renderCompactDistributedRow(surface, x, y, width, row, theme, defaultForeground, defaultBackground, font)
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

  private def renderCompactDistributedRow(
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
      val startX = x + row.leadingPadding.max(0).min(width)
      val _ = segments.zipWithIndex.foldLeft(startX) {
        case (cursorX, (segment, index)) =>
          val remainingWidth = (x + width - cursorX).max(0)
          val cellWidth      = segment.allocatedWidth.getOrElse(0).min(remainingWidth)
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
          val afterCell = cursorX + cellWidth
          val afterSeparator =
            if segment.trailingSeparator && afterCell < x + width then
              surface.setForegroundColor(defaultForeground)
              surface.setBackgroundColor(defaultBackground)
              CharacterRenderer.renderChar(surface, afterCell, y, '│')
              afterCell + 1
            else afterCell
          if index < segments.length - 1 && afterSeparator < x + width then afterSeparator + 1
          else afterSeparator
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

  private def renderInlineSegments(
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
    val rightEdge = x + width
    val _ = row.segments.foldLeft(x) { (cursorX, segment) =>
      val remainingWidth = math.max(0, rightEdge - cursorX)
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
      val nextX = cursorX + text.length
      if nextX < rightEdge then nextX + 1 else nextX
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
        val (labelWidth, hintWidth, valueWidth) = threeColumnWidths(width)
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
          font,
          alignRight = true
        )
      case label :: value :: scope :: breadcrumb :: Nil =>
        val (labelWidth, valueWidth, scopeWidth, breadcrumbWidth) = fourColumnWidths(width)
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
          valueWidth,
          value,
          theme,
          defaultForeground,
          defaultBackground,
          font
        )
        renderColumnCell(
          surface,
          x + labelWidth + valueWidth + 2,
          y,
          scopeWidth,
          scope,
          theme,
          defaultForeground,
          defaultBackground,
          font
        )
        renderColumnCell(
          surface,
          x + labelWidth + valueWidth + scopeWidth + 3,
          y,
          breadcrumbWidth,
          breadcrumb,
          theme,
          defaultForeground,
          defaultBackground,
          font,
          alignRight = true
        )
      case label :: hint :: Nil =>
        val (labelWidth, hintWidth) = twoColumnWidths(width)
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

  private def renderPriorityColumnRow(
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
      case label :: description :: shortcut :: Nil =>
        val (labelWidth, descriptionWidth, shortcutWidth) = priorityThreeColumnWidths(width)
        renderColumnCell(surface, x, y, labelWidth, label, theme, defaultForeground, defaultBackground, font)
        renderColumnCell(
          surface,
          x + labelWidth + 1,
          y,
          descriptionWidth,
          description,
          theme,
          defaultForeground,
          defaultBackground,
          font
        )
        renderColumnCell(
          surface,
          x + labelWidth + descriptionWidth + 2,
          y,
          shortcutWidth,
          shortcut,
          theme,
          defaultForeground,
          defaultBackground,
          font,
          alignRight = true
        )
      case _ =>
        renderColumnRow(surface, x, y, width, row, theme, defaultForeground, defaultBackground, font)

  private def threeColumnWidths(width: Int): (Int, Int, Int) =
    val safeWidth      = math.max(0, width)
    val preferredLabel = math.min(22, math.max(8, safeWidth / 3))
    val preferredValue = math.min(18, math.max(8, safeWidth / 4))
    val (labelWidth, valueWidth) =
      if preferredLabel + preferredValue + 2 <= safeWidth then (preferredLabel, preferredValue)
      else (math.min(22, safeWidth / 3), math.min(18, safeWidth / 4))
    (labelWidth, math.max(0, safeWidth - labelWidth - valueWidth - 2), valueWidth)

  private def priorityThreeColumnWidths(width: Int): (Int, Int, Int) =
    val safeWidth      = math.max(0, width)
    val preferredLabel = math.min(36, math.max(8, (safeWidth * 3) / 5))
    val preferredValue = math.min(18, math.max(8, safeWidth / 5))
    val (labelWidth, valueWidth) =
      if preferredLabel + preferredValue + 2 <= safeWidth then (preferredLabel, preferredValue)
      else (math.min(22, safeWidth / 3), math.min(18, safeWidth / 4))
    (labelWidth, math.max(0, safeWidth - labelWidth - valueWidth - 2), valueWidth)

  private def fourColumnWidths(width: Int): (Int, Int, Int, Int) =
    val safeWidth       = math.max(0, width)
    val labelWidth      = math.min(28, math.max(8, (safeWidth * 2) / 5))
    val valueWidth      = math.min(12, math.max(0, safeWidth / 5))
    val scopeWidth      = math.min(10, math.max(0, safeWidth / 8))
    val breadcrumbWidth = math.max(0, safeWidth - labelWidth - valueWidth - scopeWidth - 3)
    (labelWidth, valueWidth, scopeWidth, breadcrumbWidth)

  private def twoColumnWidths(width: Int): (Int, Int) =
    val safeWidth      = math.max(0, width)
    val preferredLabel = math.min(22, math.max(8, safeWidth / 3))
    val labelWidth     = if preferredLabel + 1 <= safeWidth then preferredLabel else math.min(22, safeWidth / 3)
    (labelWidth, math.max(0, safeWidth - labelWidth - 1))

  private def renderColumnCell(
    surface: RenderSurface,
    x: Int,
    y: Int,
    width: Int,
    segment: OverlaySegment,
    theme: Theme,
    defaultForeground: Color,
    defaultBackground: Color,
    font: Font,
    alignRight: Boolean = false
  ): Unit =
    val text    = fitCellText(segment.text, width)
    val renderX = if alignRight then x + math.max(0, width - text.length) else x
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
    val iconWidth   = segment.inlineIcon.map(_.length).getOrElse(0).min(width)
    val iconGap     = if iconWidth > 0 && width > iconWidth && segment.text.nonEmpty then 1 else 0
    val text        = segment.text.take(math.max(0, width - iconWidth - iconGap))
    val renderWidth = iconWidth + iconGap + text.length
    val leftPad     = math.max(0, (width - renderWidth) / 2)
    val renderX     = x + leftPad
    renderSegmentText(
      surface,
      renderX,
      y,
      renderWidth,
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
          .map(_.withAlpha(defaultBackground.getAlpha))
          .getOrElse(
            if segment.selected then theme.highlighted.background.withAlpha(defaultBackground.getAlpha)
            else if segment.tone == OverlayTone.Error then theme.error.background.withAlpha(defaultBackground.getAlpha)
            else defaultBackground
          )
      val segmentForeground =
        segment.foregroundColor
          .map(_.withAlpha(defaultForeground.getAlpha))
          .getOrElse(
            if segment.selected then theme.highlighted.foreground.withAlpha(defaultForeground.getAlpha)
            else if segment.tone == OverlayTone.Muted then theme.muted.withAlpha(defaultForeground.getAlpha)
            else if segment.tone == OverlayTone.Error then theme.error.foreground.withAlpha(defaultForeground.getAlpha)
            else defaultForeground
          )
      surface.setForegroundColor(segmentForeground)
      surface.setBackgroundColor(segmentBackground)
      val inlineIcon = segment.inlineIcon.filter(_ => width > 0)
      inlineIcon.foreach { icon =>
        segment.inlineIconFontFamily.foreach(family =>
          surface.text.setFont(Font(family, font.getStyle, font.getSize).deriveFont(font.getSize2D))
        )
        CharacterRenderer.renderStringPlain(surface, x, y, icon.take(width))
        if segment.inlineIconFontFamily.nonEmpty then surface.text.setFont(font)
      }
      val iconWidth = inlineIcon.map(_.length.min(width)).getOrElse(0)
      val iconGap   = if iconWidth > 0 && width > iconWidth && segmentText.nonEmpty then 1 else 0
      segment.fontFamily.foreach(family =>
        surface.text.setFont(Font(family, font.getStyle, font.getSize).deriveFont(font.getSize2D))
      )
      CharacterRenderer.renderStringPlain(
        surface,
        x + iconWidth + iconGap,
        y,
        segmentText.take(math.max(0, width - iconWidth - iconGap))
      )
      if segment.fontFamily.nonEmpty then surface.text.setFont(font)

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
