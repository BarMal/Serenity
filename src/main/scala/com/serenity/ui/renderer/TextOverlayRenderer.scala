package com.serenity.ui.renderer

import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.TextColor
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.SyntaxElement

object TextOverlayRenderer:

  def render(
    graphics: TextGraphics,
    overlay: TextOverlayView,
    theme: Theme,
    cursorVisible: Boolean
  ): Unit =
    val rect = overlay.rect

    graphics.setForegroundColor(theme.foregroundColor)
    graphics.setBackgroundColor(theme.backgroundColor)

    for y <- rect.y until rect.bottom do
      graphics.putString(rect.x, y, " " * rect.width)

    drawBorder(graphics, overlay)
    drawContent(graphics, overlay, theme, cursorVisible)

    graphics.setForegroundColor(theme.foregroundColor)
    graphics.setBackgroundColor(theme.backgroundColor)

  private def drawBorder(graphics: TextGraphics, overlay: TextOverlayView): Unit =
    val rect = overlay.rect

    if rect.width >= 2 && rect.height >= 2 then
      graphics.putString(rect.x, rect.y, "┌" + "─" * (rect.width - 2) + "┐")

      for y <- (rect.y + 1) until (rect.bottom - 1) do
        graphics.putString(rect.x, y, "│")
        graphics.putString(rect.right - 1, y, "│")

      graphics.putString(rect.x, rect.bottom - 1, "└" + "─" * (rect.width - 2) + "┘")

  private def drawContent(
    graphics: TextGraphics,
    overlay: TextOverlayView,
    theme: Theme,
    cursorVisible: Boolean
  ): Unit =
    val rect        = overlay.rect
    val maxLineSize = math.max(0, rect.width - 2)
    val maxLines    = math.max(0, rect.height - 2)

    val contentRows = overlay.header.toList ++ overlay.rows ++ overlay.footer.toList

    contentRows.take(maxLines).zipWithIndex.foreach { case (row, index) =>
      renderRow(
        graphics,
        rect.x + 1,
        rect.y + 1 + index,
        maxLineSize,
        row,
        theme,
        cursorVisible
      )
    }

  private def renderRow(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    width: Int,
    row: OverlayRow,
    theme: Theme,
    cursorVisible: Boolean
  ): Unit =
    val rowBackground =
      row.backgroundColor.getOrElse(
        if row.selected then theme.cursorColor else theme.backgroundColor
      )
    val rowForeground =
      row.foregroundColor.getOrElse(
        if row.selected then theme.backgroundColor else theme.foregroundColor
      )

    graphics.setForegroundColor(rowForeground)
    graphics.setBackgroundColor(rowBackground)
    CharacterRenderer.renderStringPlain(graphics, x, y, " " * width)

    row.layout match
      case OverlayRowLayout.Plain =>
        CharacterRenderer.renderStringPlain(graphics, x, y, row.plainText.take(width))
      case OverlayRowLayout.Distributed =>
        renderDistributedRow(graphics, x, y, width, row, theme, rowForeground, rowBackground)
      case OverlayRowLayout.Split =>
        renderSplitRow(graphics, x, y, width, row, theme, rowForeground, rowBackground)

    if cursorVisible then
      row.cursorColumn.foreach { cursorColumn =>
        if cursorColumn >= 0 && cursorColumn < width then
          val cursorX = x + cursorColumn
          val cursorChar =
            if cursorColumn < row.plainText.length then row.plainText.charAt(cursorColumn)
            else ' '
          graphics.setForegroundColor(theme.backgroundColor)
          graphics.setBackgroundColor(theme.cursorColor)
          CharacterRenderer.renderChar(graphics, cursorX, y, cursorChar)
      }

  private def renderDistributedRow(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    width: Int,
    row: OverlayRow,
    theme: Theme,
    defaultForeground: TextColor,
    defaultBackground: TextColor
  ): Unit =
    val segments = row.segments
    if segments.isEmpty then
      CharacterRenderer.renderStringPlain(graphics, x, y, row.plainText.take(width))
    else
      val baseCellWidth = width / segments.length
      val remainder     = width % segments.length

      segments.zipWithIndex.foldLeft(x) { case (cursorX, (segment, index)) =>
        val cellWidth = baseCellWidth + (if index < remainder then 1 else 0)
        renderSegmentCell(graphics, cursorX, y, cellWidth, segment, theme, defaultForeground, defaultBackground)
        cursorX + cellWidth
      }
      ()

  private def renderSplitRow(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    width: Int,
    row: OverlayRow,
    theme: Theme,
    defaultForeground: TextColor,
    defaultBackground: TextColor
  ): Unit =
    row.segments match
      case left :: rightSegments if rightSegments.nonEmpty =>
        val rightTexts = rightSegments.map(_.text)
        val rightGroupText = rightTexts.mkString(" ")
        val rightGroupWidth = math.min(width, rightGroupText.length)
        val leftMaxWidth = math.max(0, width - rightGroupWidth - 1)
        val leftText = left.text.take(leftMaxWidth)

        renderSegmentText(graphics, x, y, leftText.length, leftText, left, theme, defaultForeground, defaultBackground)

        val rightStartX = x + math.max(0, width - rightGroupWidth)
        rightSegments.foldLeft(rightStartX) { (cursorX, segment) =>
          val text = segment.text.take(math.max(0, x + width - cursorX))
          renderSegmentText(graphics, cursorX, y, text.length, text, segment, theme, defaultForeground, defaultBackground)
          cursorX + text.length + 1
        }
      case _ =>
        CharacterRenderer.renderStringPlain(graphics, x, y, row.plainText.take(width))

  private def renderSegmentCell(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    width: Int,
    segment: OverlaySegment,
    theme: Theme,
    defaultForeground: TextColor,
    defaultBackground: TextColor
  ): Unit =
    val text      = segment.text.take(width)
    val leftPad   = math.max(0, (width - text.length) / 2)
    val renderX   = x + leftPad
    renderSegmentText(graphics, renderX, y, text.length, text, segment, theme, defaultForeground, defaultBackground)

  private def renderSegmentText(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    width: Int,
    segmentText: String,
    segment: OverlaySegment,
    theme: Theme,
    defaultForeground: TextColor,
    defaultBackground: TextColor
  ): Unit =
    if width > 0 then
      val segmentBackground =
        segment.backgroundColor.getOrElse(
          if segment.selected then theme.cursorColor else defaultBackground
        )
      val segmentForeground =
        segment.foregroundColor.getOrElse(
          if segment.selected then theme.backgroundColor
          else if segment.tone == OverlayTone.Muted then theme.colorFor(SyntaxElement.Comment).foreground
          else defaultForeground
        )
      graphics.setForegroundColor(segmentForeground)
      graphics.setBackgroundColor(segmentBackground)
      CharacterRenderer.renderStringPlain(graphics, x, y, segmentText.take(width))
