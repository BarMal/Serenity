package com.serenity.ui.layout

import com.serenity.config.TabMaxWidth
import com.serenity.state.models.BufferId

/** Which tabs have a close (x) box. The active tab always has one. The layout never depends on this: an inactive tab's
  * `x` is drawn in the right padding cell its tab already has, so showing or hiding it moves nothing.
  */
enum CloseReveal:
  case ActiveOnly
  case Hovered(bufferId: BufferId)
  case All

/** How wide a tab title is in cells, and how it is cut when there is not room for it.
  *
  * `cellsOf` is the title's width on the strip's cell grid. `ellipsis` ends a cut title. `minTitleCells` is the least
  * room a squeezed title keeps, always five letters and the ellipsis: 6 cells with the one-cell `…`, 8 with the
  * three-dot ellipsis of a cell-grid terminal.
  */
final case class TabTitleMeasure(cellsOf: String => Int, ellipsis: String, minTitleCells: Int):

  def cells(label: String): Int = cellsOf(label)

  /** `label` if it fits in `cells`, else its longest start that fits together with the ellipsis. Where not even the
    * ellipsis fits, the first `cells` characters, and never fewer than one.
    */
  def fit(label: String, cells: Int): String =
    if cellsOf(label) <= cells then label
    else
      Iterator
        .range(label.length - 1, -1, -1)
        .map(length => label.take(length) + ellipsis)
        .find(cellsOf(_) <= cells)
        .getOrElse(label.take(math.max(1, cells)))

object TabTitleMeasure:

  /** A cell grid's measure: a title is as many cells as it has characters, cut with "...". */
  val CellGrid: TabTitleMeasure = TabTitleMeasure(_.length, "...", 8)

  /** A proportional font's: a title is as many cells as its advance, and the text inset it is drawn with, take. */
  def proportional(advancePx: String => Double, insetPx: Double, cellWidthPx: Double): TabTitleMeasure =
    TabTitleMeasure(
      label => math.ceil((advancePx(label) + insetPx) / math.max(1.0, cellWidthPx)).toInt,
      "…",
      6
    )

/** Everything `TabBarSurfaceComposition` needs besides the entries and the strip's rect. */
final case class TabBarSettings(
    maxTabWidth: Int = TabMaxWidth.Default,
    measure: TabTitleMeasure = TabTitleMeasure.CellGrid,
    reveal: CloseReveal = CloseReveal.ActiveOnly
)

object TabBarSettings:

  val Default: TabBarSettings = TabBarSettings()
