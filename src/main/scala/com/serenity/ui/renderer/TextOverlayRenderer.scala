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

    if config.surfaceConfig.uiShadowsEnabled then
      surface.roundedRects.foreach(
        _.drawRoundRectShadow(rect.x, rect.y, rect.width, rect.height, config.uiCornerRadiusPx, new Color(0, 0, 0))
      )

    // Scoped to the one surface named by config.surfaceConfig.cursorInfoBarBackgroundAlpha's doc comment -- every
    // other floating panel keeps painting with theme.panel.background exactly as before, unmodified.
    val cursorInfoBarBackgroundAlphaOverride: Option[Int] =
      Option
        .when(overlay.surfaceId.contains(UiSurface.CursorInfoBarSurfaceId))(
          config.surfaceConfig.cursorInfoBarBackgroundAlpha
        )
        .flatten
        .map(alpha => math.round(alpha * 255.0).toInt.max(0).min(255))

    // #1295: same one-surface scoping as the alpha override above, for the cursor info bar's own foreground/
    // background colour instead of just its background alpha.
    val isCursorInfoBar = overlay.surfaceId.contains(UiSurface.CursorInfoBarSurfaceId)
    val cursorInfoBarForegroundOverride: Option[Color] =
      Option.when(isCursorInfoBar)(config.cursorInfoBarColors.foreground).flatten
    val cursorInfoBarBackgroundOverride: Option[Color] =
      Option.when(isCursorInfoBar)(config.cursorInfoBarColors.background).flatten

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
      val fg = cursorInfoBarForegroundOverride.getOrElse(defaultFg)
      val bg = cursorInfoBarBackgroundOverride.getOrElse(defaultBg)
      (fg, cursorInfoBarBackgroundAlphaOverride.fold(bg)(bg.withAlpha))

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
