package com.serenity.ui.layout

import com.serenity.richtext.{ParagraphRole, RichTextParagraph}
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.openings.OpeningInvalidation

/** Diffs two consecutive frames of one editor pane's text layout and reports which visible rows can no longer reuse the
  * pixels an earlier frame left on the surface.
  *
  * The function is deliberately pure data in, plain row indices out: it has no knowledge of Java2D, surfaces or the
  * renderer, so the rule that governs screen correctness can be tested with fixtures alone.
  *
  * The bias is always towards reporting too much. Redrawing a row that did not change is invisible; skipping a row that
  * did change leaves stale pixels on screen. Every whole-snapshot property that shifts row geometry (panel width, row
  * height, ascent, proportional/measured layout mode, how inline Markdown is restyled) therefore invalidates every row
  * rather than being reasoned about per row. The rich-text document that drives styling is the exception (#1855): it is
  * compared paragraph by paragraph for the rows on screen, so typing in one paragraph repaints only that paragraph.
  */
object DirtyLineDiff:

  /** Rows of `current` whose pixels may differ from `previous`, comparing text layout only.
    *
    * A row is dirty when the previous frame had no row at that index, when its [[TextVisualLine]] differs in any field,
    * or when a snapshot-wide property changed. `None` for `previous` means there is no reusable prior frame, so every
    * row is dirty.
    */
  def dirtyRows(previous: Option[TextLayoutSnapshot], current: TextLayoutSnapshot): Set[Int] =
    previous match
      case None => allRows(current)
      case Some(previousSnapshot) =>
        if !comparableShape(previousSnapshot, current) then allRows(current)
        else
          val reflowFrom =
            List(
              richReflowStart(previousSnapshot, current),
              extentReflowStart(previousSnapshot, current)
            ).flatten.minOption
          val changed = current.visualLines.indices.filter { row =>
            reflowFrom.exists(row >= _) ||
            !previousSnapshot.visualLines.lift(row).contains(current.visualLines(row)) ||
            richParagraphEdited(previousSnapshot, current, current.visualLines(row).bufferLine) ||
            openingGovernorEdited(previousSnapshot, current, current.visualLines(row).bufferLine)
          }.toSet
          closedOverGlyphs(changed, previousSnapshot, current)

  /** As [[dirtyRows]], additionally comparing a caller-supplied style key per row.
    *
    * The style key carries everything outside the text layout that feeds a row's pixels — selection spans, cell
    * animations, cursors drawn onto this surface, comment highlights, focused-body dimming. Keys are matched positional
    * by row index; if either key vector does not line up one-to-one with its snapshot's rows the diff gives up and
    * marks every row dirty.
    */
  def dirtyRows[K](
    previous: Option[TextLayoutSnapshot],
    current: TextLayoutSnapshot,
    previousRowKeys: Vector[K],
    currentRowKeys: Vector[K]
  ): Set[Int] =
    val layoutDirty = dirtyRows(previous, current)
    previous match
      case None => layoutDirty
      case Some(previousSnapshot) =>
        val alignedKeys =
          previousRowKeys.length == previousSnapshot.visualLines.length &&
            currentRowKeys.length == current.visualLines.length
        if !alignedKeys then allRows(current)
        else
          closedOverGlyphs(
            layoutDirty ++ current.visualLines.indices.filter { row =>
              !previousRowKeys.lift(row).contains(currentRowKeys(row))
            },
            previousSnapshot,
            current
          )
    end match

  /** Grow a dirty set by one row in each direction, bounded to `rowCount`.
    *
    * Glyphs are clipped to the surface grid rather than to their own row, so an ascender or descender on a redrawn row
    * can paint a few pixels into the band of the row above or below. Dilating the dirty set means those neighbours are
    * redrawn too, instead of keeping a preserved band that the neighbour's overflow no longer reaches.
    */
  def dilate(rows: Set[Int], rowCount: Int): Set[Int] =
    if rows.isEmpty then Set.empty
    else rows.flatMap(row => Set(row - 1, row, row + 1)).filter(row => row >= 0 && row < rowCount)

  /** A drop cap glyph is one opaque box painted by its first row over every row it spans, so a span with any dirty row
    * is dirty whole, in the frame being drawn and in the one whose pixels are kept.
    */
  def closedOverGlyphs(rows: Set[Int], previous: TextLayoutSnapshot, current: TextLayoutSnapshot): Set[Int] =
    DropCapLayout
      .closedOverGlyphs(DropCapLayout.closedOverGlyphs(rows, previous), current)
      .filter(row => row < current.visualLines.length)

  private def allRows(snapshot: TextLayoutSnapshot): Set[Int] =
    snapshot.visualLines.indices.toSet

  private def sameRichParagraphCount(previous: TextLayoutSnapshot, current: TextLayoutSnapshot): Boolean =
    (previous.richTextDocument, current.richTextDocument) match
      case (None, None)                => true
      case (Some(before), Some(after)) => (before eq after) || before.paragraphCount == after.paragraphCount
      case _                           => false

  private def richParagraphEdited(previous: TextLayoutSnapshot, current: TextLayoutSnapshot, line: Int): Boolean =
    (previous.richTextDocument, current.richTextDocument) match
      case (Some(before), Some(after)) if !(before eq after) =>
        (before.paragraphAt(line), after.paragraphAt(line)) match
          case (Some(was), Some(now)) => withoutRole(was) != withoutRole(now) || roleDiffers(previous, current, line)
          case (None, None)           => false
          case _                      => true
      case _ => false

  /** A paragraph can gain or lose chapter-opening status when the title, scene break or epigraph above it is edited,
    * without itself changing.
    */
  private def openingGovernorEdited(previous: TextLayoutSnapshot, current: TextLayoutSnapshot, line: Int): Boolean =
    (previous.richTextDocument, current.richTextDocument) match
      case (Some(before), Some(after)) if !(before eq after) => OpeningInvalidation.governorEdited(before, after, line)
      case _                                                 => false

  /** The first visible row below which rows may have moved without their own text changing: a paragraph whose role or
    * alignment changed, or a row whose measured height or ascent did. Rows above it keep their position.
    */
  private def richReflowStart(previous: TextLayoutSnapshot, current: TextLayoutSnapshot): Option[Int] =
    (previous.richTextDocument, current.richTextDocument) match
      case (Some(before), Some(after)) if !(before eq after) =>
        current.visualLines.indices.find { row =>
          val line = current.visualLines(row)
          previous.visualLines
            .lift(row)
            .exists(old => old.heightPx != line.heightPx || old.ascentPx != line.ascentPx) ||
          formattingDiffers(previous, current, line.bufferLine)
        }
      case _ => None

  /** The first visible row whose band is taller or shorter than the row it replaces -- a heading's space gained or
    * lost, say. Rows below it keep their text but not their place, and are dirty with it however their own lines
    * compare.
    */
  private def extentReflowStart(previous: TextLayoutSnapshot, current: TextLayoutSnapshot): Option[Int] =
    current.visualLines.indices.find { row =>
      previous.visualLines.lift(row).exists(old => extentOf(old) != extentOf(current.visualLines(row)))
    }

  private def extentOf(line: TextVisualLine): Int =
    line.spaceBeforePx + line.heightPx + line.reservedBelowPx + line.spaceAfterPx

  private def formattingDiffers(previous: TextLayoutSnapshot, current: TextLayoutSnapshot, line: Int): Boolean =
    (
      previous.richTextDocument.flatMap(_.paragraphAt(line)),
      current.richTextDocument.flatMap(_.paragraphAt(line))
    ) match
      case (Some(was), Some(now)) => roleDiffers(previous, current, line) || was.alignment != now.alignment
      case (None, None)           => false
      case _                      => true

  private def roleDiffers(previous: TextLayoutSnapshot, current: TextLayoutSnapshot, line: Int): Boolean =
    (previous.richTextDocument, current.richTextDocument) match
      case (Some(before), Some(after)) =>
        previous.roles.roleFor(before, line) != current.roles.roleFor(after, line)
      case _ => false

  private def withoutRole(paragraph: RichTextParagraph): RichTextParagraph =
    paragraph.copy(role = ParagraphRole.Body)

  /** Whether rows of `previous` and `current` can be compared at all: nothing about the whole snapshot moved. */
  def comparableShape(previous: TextLayoutSnapshot, current: TextLayoutSnapshot): Boolean =
    previous.panelWidthPx == current.panelWidthPx &&
      previous.lineHeightPx == current.lineHeightPx &&
      previous.ascentPx == current.ascentPx &&
      previous.isProportional == current.isProportional &&
      previous.usesMeasuredLayout == current.usesMeasuredLayout &&
      previous.roles.context.virtualDropCaps.map(_.signature) ==
      current.roles.context.virtualDropCaps.map(_.signature) &&
      sameRichParagraphCount(previous, current) &&
      previous.markdownInline.mode == current.markdownInline.mode &&
      previous.markdownInline.padsAlike(current.markdownInline, current.visualLines.map(_.bufferLine).distinct)
