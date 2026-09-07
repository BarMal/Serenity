package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme

/** Paints the column-oriented [[OverlayRowLayout]] variants -- `Columns` and `PriorityColumns` -- and owns the column
  * geometry they lay cells out with.
  *
  * [[threeColumnWidths]] and [[fitCellText]] are public for that second reason: [[TextOverlayRenderer]] places the
  * caret inside a selected column cell, and can only land it on the right pixel by measuring the cell exactly as this
  * object drew it. A private copy of either rule in the cursor code is a caret that drifts off its cell the moment the
  * column widths are tuned.
  */
object OverlayColumnRowRenderer:

  def renderColumnRow(
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

  def renderPriorityColumnRow(
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

  def threeColumnWidths(width: Int): (Int, Int, Int) =
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
    OverlaySegmentRowRenderer.renderSegmentText(
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

  def fitCellText(text: String, width: Int): String =
    if width <= 0 then ""
    else if text.length <= width then text
    else if width <= 3 then text.take(width)
    else text.take(width - 3) + "..."
