package com.serenity.ui.renderer

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{EditorLayoutContract, LayoutRect, RoundedCorners}
import com.serenity.ui.theme.Theme

/** What a frame drawn from scratch has under each corner of a rounded element. A rounded fill is anti-aliased, and a
  * frame that repaints the element over its last frame's pixels would composite the fringe a second time; with the
  * underlay the fill restores the corner's exterior first, so a repaint starts from what a cold frame starts from.
  */
final case class CornerUnderlay(
    topLeft: Option[RenderColor],
    topRight: Option[RenderColor],
    bottomRight: Option[RenderColor],
    bottomLeft: Option[RenderColor]
)

object CornerUnderlay:

  val absent: CornerUnderlay = CornerUnderlay(None, None, None, None)

  def all(color: RenderColor): CornerUnderlay = CornerUnderlay(Some(color), Some(color), Some(color), Some(color))

  /** Under the corners of `rect` that `corners` rounds: the margin where a side spacer column lies under the corner
    * (the spacers are painted before the panes), the window's background everywhere else.
    */
  def over(rect: LayoutRect, corners: RoundedCorners, contract: EditorLayoutContract, theme: Theme): CornerUnderlay =
    val spacers = contract.sideSpacerRects
    def under(rounded: Boolean, cellX: Int, cellY: Int): Option[RenderColor] =
      Option.when(rounded):
        if spacers.exists(spacer => lies(spacer, cellX, cellY)) then theme.margin else theme.background
    CornerUnderlay(
      under(corners.topLeft, rect.x, rect.y),
      under(corners.topRight, rect.right - 1, rect.y),
      under(corners.bottomRight, rect.right - 1, rect.bottom - 1),
      under(corners.bottomLeft, rect.x, rect.bottom - 1)
    )

  private def lies(spacer: LayoutRect, cellX: Int, cellY: Int): Boolean =
    spacer.width > 0 && spacer.height > 0 && cellX >= spacer.x && cellX < spacer.right && cellY >= spacer.y &&
      cellY < spacer.bottom
