package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.serenity.ui.theme.Theme
import java.awt.Color

object TextOverlayRenderer:

  def render(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    cursorVisible: Boolean
  ): Unit =
    val rect = overlay.rect

    def rowColors(rowOffset: Int): (Color, Color) =
      overlay.animationState.getCell(0, rowOffset)
        .map(cell =>
          ( cell.currentForeground.getOrElse(theme.panel.foreground.toColor())
          , cell.currentBackground.getOrElse(theme.panel.background.toColor())
          )
        )
        .getOrElse((theme.panel.foreground.toColor(), theme.panel.background.toColor()))

    surface.setAlpha(theme.panel.alpha.toFloat)

    for (y, rowOffset) <- (rect.y until rect.bottom).zipWithIndex do
      val (fg, bg) = rowColors(rowOffset)
      surface.setForegroundColor(fg)
      surface.setBackgroundColor(bg)
      surface.putString(rect.x, y, " " * rect.width)

    val animated = overlay.animationState.animations.nonEmpty
    drawBorder(surface, overlay, theme, rowColors)
    drawContent(surface, overlay, theme, cursorVisible, rowColors, animated)

    surface.setAlpha(1.0f)
    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)

  private def drawBorder(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    rowColors: Int => (Color, Color)
  ): Unit =
    val rect      = overlay.rect
    val animated  = overlay.animationState.animations.nonEmpty

    if rect.width >= 2 && rect.height >= 2 then
      val (topFg, topBg) = rowColors(0)
      surface.setForegroundColor(if animated then topFg else theme.border.toColor())
      surface.setBackgroundColor(topBg)
      surface.putString(rect.x, rect.y, "+" + "-" * (rect.width - 2) + "+")

      for y <- (rect.y + 1) until (rect.bottom - 1) do
        val rowOff           = y - rect.y
        val (sideFg, sideBg) = rowColors(rowOff)
        surface.setForegroundColor(if animated then sideFg else theme.border.toColor())
        surface.setBackgroundColor(sideBg)
        surface.putString(rect.x, y, "|")
        surface.putString(rect.right - 1, y, "|")

      val (botFg, botBg) = rowColors(rect.height - 1)
      surface.setForegroundColor(if animated then botFg else theme.border.toColor())
      surface.setBackgroundColor(botBg)
      surface.putString(rect.x, rect.bottom - 1, "+" + "-" * (rect.width - 2) + "+")

  private def drawContent(
    surface: RenderSurface,
    overlay: TextOverlayView,
    theme: Theme,
    cursorVisible: Boolean,
    rowColors: Int => (Color, Color),
    animated: Boolean
  ): Unit =
    val rect        = overlay.rect
    val maxLineSize = math.max(0, rect.width - 2)
    val maxLines    = math.max(0, rect.height - 2)

    val contentRows = overlay.header.toList ++ overlay.rows ++ overlay.footer.toList

    contentRows.take(maxLines).zipWithIndex.foreach { case (row, index) =>
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
        isAnimating = animated
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
    isAnimating: Boolean = false
  ): Unit =
    val baseFg = if defaultForeground != null then defaultForeground else theme.panel.foreground.toColor()
    val baseBg = if defaultBackground != null then defaultBackground else theme.panel.background.toColor()
    val rowBackground =
      if isAnimating then baseBg
      else row.backgroundColor.map(_.toColor()).getOrElse(
        if row.selected then theme.highlighted.background.toColor() else baseBg
      )
    val rowForeground =
      if isAnimating then baseFg
      else row.foregroundColor.map(_.toColor()).getOrElse(
        if row.selected then theme.highlighted.foreground.toColor() else baseFg
      )

    surface.setForegroundColor(rowForeground)
    surface.setBackgroundColor(rowBackground)
    CharacterRenderer.renderStringPlain(surface, x, y, " " * width)

    row.layout match
      case OverlayRowLayout.Plain =>
        CharacterRenderer.renderStringPlain(surface, x, y, row.plainText.take(width))
      case OverlayRowLayout.Distributed =>
        renderDistributedRow(surface, x, y, width, row, theme, rowForeground, rowBackground, isAnimating)
      case OverlayRowLayout.Split =>
        renderSplitRow(surface, x, y, width, row, theme, rowForeground, rowBackground, isAnimating)

    if cursorVisible then
      row.cursorColumn.foreach { cursorColumn =>
        if cursorColumn >= 0 && cursorColumn < width then
          val cursorX = x + cursorColumn
          val cursorChar =
            if cursorColumn < row.plainText.length then row.plainText.charAt(cursorColumn)
            else ' '
          surface.setForegroundColor(theme.background.toColor())
          surface.setBackgroundColor(theme.cursor.toColor())
          CharacterRenderer.renderChar(surface, cursorX, y, cursorChar)
      }

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
    if segments.isEmpty then
      CharacterRenderer.renderStringPlain(surface, x, y, row.plainText.take(width))
    else
      val baseCellWidth = width / segments.length
      val remainder     = width % segments.length

      segments.zipWithIndex.foldLeft(x) { case (cursorX, (segment, index)) =>
        val cellWidth = baseCellWidth + (if index < remainder then 1 else 0)
        renderSegmentCell(surface, cursorX, y, cellWidth, segment, theme, defaultForeground, defaultBackground, isAnimating)
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

        renderSegmentText(surface, x, y, leftText.length, leftText, left, theme, defaultForeground, defaultBackground, isAnimating)

        val rightStartX = x + math.max(0, width - rightGroupWidth)
        rightSegments.foldLeft(rightStartX) { (cursorX, segment) =>
          val text = segment.text.take(math.max(0, x + width - cursorX))
          renderSegmentText(surface, cursorX, y, text.length, text, segment, theme, defaultForeground, defaultBackground, isAnimating)
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
    renderSegmentText(surface, renderX, y, text.length, text, segment, theme, defaultForeground, defaultBackground, isAnimating)

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
        if isAnimating then defaultBackground
        else segment.backgroundColor.map(_.toColor()).getOrElse(
          if segment.selected then theme.highlighted.background.toColor()
          else if segment.tone == OverlayTone.Error then theme.error.background.toColor()
          else defaultBackground
        )
      val segmentForeground =
        if isAnimating then defaultForeground
        else segment.foregroundColor.map(_.toColor()).getOrElse(
          if segment.selected then theme.highlighted.foreground.toColor()
          else if segment.tone == OverlayTone.Muted then theme.muted.toColor()
          else if segment.tone == OverlayTone.Error then theme.error.foreground.toColor()
          else defaultForeground
        )
      surface.setForegroundColor(segmentForeground)
      surface.setBackgroundColor(segmentBackground)
      CharacterRenderer.renderStringPlain(surface, x, y, segmentText.take(width))
