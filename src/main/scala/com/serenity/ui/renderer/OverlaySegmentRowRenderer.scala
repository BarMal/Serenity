package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.ui.layout.*
import com.serenity.ui.theme.ColorFormat.withAlpha
import com.serenity.ui.theme.Theme

/** Paints the segment-oriented [[OverlayRowLayout]] variants -- `Distributed`, `Split`, and `Plain`'s inline-segment
  * path -- extracted from [[TextOverlayRenderer]] alongside [[OverlayColumnRowRenderer]] to keep each row-layout family
  * in one place.
  */
object OverlaySegmentRowRenderer:

  def renderDistributedRow(
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

  def renderCompactDistributedRow(
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

  def renderSplitRow(
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

  def renderEditableSplitRow(
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

  def renderInlineSegments(
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

  def renderSegmentCell(
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

  def renderSegmentText(
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
