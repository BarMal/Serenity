package com.serenity.ui.layout

import com.serenity.state.models.ConfirmPrompt
import com.serenity.ui.widget.ButtonEmphasis

/** The one composition every [[ConfirmPrompt]] is drawn with: its title, its message lines, then one action row per
  * choice, the highlighted choice selected.
  */
object ConfirmComposition:

  private val ActionIdPrefix = "confirm-choice-"

  def choiceActionId(index: Int): SurfaceActionId = SurfaceActionId(s"$ActionIdPrefix$index")

  /** The choice a hit region's action id names -- only ever one [[choiceActionId]] produced. */
  def choiceIndex(actionId: String): Option[Int] =
    Option.when(actionId.startsWith(ActionIdPrefix))(actionId.stripPrefix(ActionIdPrefix)).flatMap(_.toIntOption)

  /** Choices stack one per row when the frame has room for them under the text; a frame too short for that keeps as
    * much text as fits and lays the choices side by side along its bottom row, so a cramped terminal still offers every
    * answer.
    */
  def forPrompt(prompt: ConfirmPrompt, frameRect: LayoutRect, targetRows: Int): ResolvedSurfaceComposition =
    val content     = SurfaceFrameLayout(frameRect).contentRect
    val actionRows  = math.max(1, targetRows)
    val bounds      = ModalSurfaceComposition.logicalRect(content.x, content.y, content.width, content.height)
    val lines       = textLines(prompt)
    val choiceCount = prompt.choices.items.size
    val stacked     = content.height >= lines.length + choiceCount * actionRows
    val sideBySide  = !stacked && content.height >= 1 && content.width >= choiceCount
    val shownLines =
      if stacked then lines
      else if sideBySide then lines.takeRight(math.max(0, content.height - 1))
      else Nil
    val textBoxes = shownLines.zipWithIndex.map { (text, index) =>
      ModalSurfaceComposition.textBox(text, ModalSurfaceComposition.rowRect(bounds, index))
    }
    val actionRects =
      if sideBySide then sideBySideRects(content, choiceCount)
      else
        (0 until choiceCount).toList.map(index =>
          ModalSurfaceComposition.logicalRect(
            content.x,
            content.y + shownLines.length + index * actionRows,
            content.width,
            actionRows
          )
        )
    val actionBoxes = prompt.choices.items.toList.zip(actionRects).zipWithIndex.map {
      case ((choice, rect), index) =>
        ModalSurfaceComposition.actionBox(
          choice.label,
          choiceActionId(index),
          SurfaceFocusId(choiceActionId(index).value),
          selected = prompt.choices.selected.contains(index),
          rect,
          toneFor(choice.emphasis)
        )
    }
    val clippedTextBoxes   = textBoxes.flatMap(ModalSurfaceComposition.clipBox(_, bounds))
    val clippedActionBoxes = actionBoxes.flatMap(ModalSurfaceComposition.clipBox(_, bounds))
    val hitRegions = clippedActionBoxes.flatMap { box =>
      for
        focusId       <- box.focusId
        actionId      <- box.actionId
        semanticLabel <- box.semanticLabel
      yield SurfaceHitRegion(box.rect, focusId, Some(actionId), semanticLabel)
    }
    ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize = SurfaceIntrinsicSize(
        width = (lines ++ prompt.choices.items.map(_.label)).map(_.length.toDouble).foldLeft(0.0)(_ max _),
        height = lines.length + choiceCount * actionRows
      ),
      paintBoxes = clippedTextBoxes ++ clippedActionBoxes,
      hitRegions = hitRegions,
      focusOrder = clippedActionBoxes.flatMap(_.focusId)
    )

  /** `count` cells of the content's bottom row, as even as whole cells allow -- the leftmost take the remainder. */
  private def sideBySideRects(content: LayoutRect, count: Int): List[LogicalPixelRect] =
    val baseWidth  = content.width / count
    val extraCells = content.width % count
    (0 until count).toList
      .foldLeft((content.x, List.empty[LogicalPixelRect])) {
        case ((nextX, rects), index) =>
          val width = baseWidth + (if index < extraCells then 1 else 0)
          (nextX + width, rects :+ ModalSurfaceComposition.logicalRect(nextX, content.bottom - 1, width, 1))
      }
      ._2

  def frameHeight(prompt: ConfirmPrompt, targetRows: Int): Int =
    SurfaceFrameLayout.DefaultBorderCells * 2 + textLines(prompt).length +
      prompt.choices.items.size * math.max(1, targetRows)

  private def textLines(prompt: ConfirmPrompt): List[String] = prompt.title :: prompt.message

  private def toneFor(emphasis: ButtonEmphasis): OverlayTone =
    emphasis match
      case ButtonEmphasis.Primary   => OverlayTone.Accent
      case ButtonEmphasis.Secondary => OverlayTone.Normal
      case ButtonEmphasis.Danger    => OverlayTone.Error
