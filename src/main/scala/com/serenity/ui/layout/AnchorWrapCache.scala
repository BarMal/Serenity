package com.serenity.ui.layout

/** The wraps measured while locating a caret. The layout is a function of the state alone, so a state keeps its own
  * rather than borrowing the scene's.
  *
  * It has no parameters so that every one is equal to every other, as [[com.serenity.markdown.MarkdownBlockIndexMemo]]
  * does: what the cache holds never changes what the state carrying it means.
  */
final case class AnchorWrapCache():

  val lines: WrappedLineCache.Bounded = WrappedLineCache.bounded()
