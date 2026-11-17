package com.serenity.ui.layout

/** The wraps measured while locating a caret. The layout is a function of the state alone, so a state keeps its own
  * rather than borrowing the scene's.
  *
  * It has no parameters of its own so that every one is equal to every other, as
  * [[com.serenity.markdown.MarkdownBlockIndexMemo]] does: what the cache holds never changes what the state carrying it
  * means. It measures with the owner's measurer, given in the second list, so a popup anchored to a caret lands on the
  * row the renderer paints it on; with none it lays out on the cell grid.
  */
final case class AnchorWrapCache()(measurerSource: () => Option[TextMeasurer] = () => None):

  val lines: WrappedLineCache.Bounded =
    WrappedLineCache.bounded(layoutMemos = LayoutMemos(measurerSource = measurerSource))
