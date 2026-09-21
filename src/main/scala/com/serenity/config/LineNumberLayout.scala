package com.serenity.config

/** Which edge(s) of the editor carry the line-number counter. */
enum LineNumberSide:
  case Left
  case Right
  case Both

  def showsLeft: Boolean =
    this == LineNumberSide.Left || this == LineNumberSide.Both

  def showsRight: Boolean =
    this == LineNumberSide.Right || this == LineNumberSide.Both

  def configKey: String =
    this match
      case LineNumberSide.Left  => "left"
      case LineNumberSide.Right => "right"
      case LineNumberSide.Both  => "both"

object LineNumberSide:
  def fromConfigKey(value: String): Option[LineNumberSide] =
    values.find(_.configKey.equalsIgnoreCase(value))

/** Placement and spacing of the line-number counter, independent of interface density.
  *
  * All values are in cells, so they scale with the font in the GUI (a cell is font-sized there). `margin` is the gap
  * from the panel edge to the counter on a side -- applied even when that side carries no counter, so toggling the
  * counter on a side does not shift content. `padding` is the gap between the counter and the editor content, applied
  * only on sides that actually have a counter.
  */
final case class LineNumberLayout(
    side: LineNumberSide = LineNumberSide.Left,
    // `None` means the user never set this: resolved by a surface-aware default (GUI a cell, TUI the flush-to-edge
    // density it always had), rather than baking either surface's default in here -- see
    // `AppState.effectiveLineNumberMarginLeft`. `Some` is honoured on both surfaces unchanged. `marginRight` carries
    // no such default and stays a plain `Int` -- only the left margin and the padding were flagged as reading flush.
    marginLeft: Option[Int] = None,
    marginRight: Int = 0,
    padding: Option[Int] = None
):

  def normalized: LineNumberLayout =
    copy(
      marginLeft = marginLeft.map(LineNumberLayout.clampCells),
      marginRight = LineNumberLayout.clampCells(marginRight),
      padding = padding.map(LineNumberLayout.clampCells)
    )

object LineNumberLayout:
  // Sanity ceiling against a fat-fingered value, not a spacing anyone would actually want; a counter margin/padding
  // wider than this would swallow the whole workspace on a small viewport.
  val MaxCells: Int = 32

  def clampCells(cells: Int): Int =
    cells.max(0).min(MaxCells)
