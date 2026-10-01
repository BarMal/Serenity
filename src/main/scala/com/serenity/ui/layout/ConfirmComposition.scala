package com.serenity.ui.layout

import com.serenity.state.models.ConfirmPrompt

/** The one composition every [[ConfirmPrompt]] is drawn with: its title, its message lines, then one action row per
  * choice, the highlighted choice selected.
  */
object ConfirmComposition:

  private val ActionIdPrefix = "confirm-choice-"

  def choiceActionId(index: Int): SurfaceActionId = SurfaceActionId(s"$ActionIdPrefix$index")

  /** The choice a hit region's action id names -- only ever one [[choiceActionId]] produced. */
  def choiceIndex(actionId: String): Option[Int] =
    Option.when(actionId.startsWith(ActionIdPrefix))(actionId.stripPrefix(ActionIdPrefix)).flatMap(_.toIntOption)

  def forPrompt(prompt: ConfirmPrompt, frameRect: LayoutRect, targetRows: Int): ResolvedSurfaceComposition =
    val content    = SurfaceFrameLayout(frameRect).contentRect
    val actionRows = math.max(1, targetRows)
    val bounds     = ModalSurfaceComposition.logicalRect(content.x, content.y, content.width, content.height)
    val lines      = textLines(prompt)
    val textBoxes = lines.zipWithIndex.map { (text, index) =>
      ModalSurfaceComposition.textBox(text, ModalSurfaceComposition.rowRect(bounds, index))
    }
    val actionStartY = content.y + lines.length
    val actionBoxes = prompt.choices.items.toList.zipWithIndex.map { (choice, index) =>
      ModalSurfaceComposition.actionBox(
        choice.label,
        choiceActionId(index),
        SurfaceFocusId(choiceActionId(index).value),
        selected = prompt.choices.selected.contains(index),
        ModalSurfaceComposition.logicalRect(content.x, actionStartY + index * actionRows, content.width, actionRows)
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
        height = lines.length + prompt.choices.items.size * actionRows
      ),
      paintBoxes = clippedTextBoxes ++ clippedActionBoxes,
      hitRegions = hitRegions,
      focusOrder = clippedActionBoxes.flatMap(_.focusId)
    )

  def frameHeight(prompt: ConfirmPrompt, targetRows: Int): Int =
    SurfaceFrameLayout.DefaultBorderCells * 2 + textLines(prompt).length +
      prompt.choices.items.size * math.max(1, targetRows)

  private def textLines(prompt: ConfirmPrompt): List[String] = prompt.title :: prompt.message
