package com.serenity.ui.layout

import java.text.BreakIterator
import java.util.Locale

/** The break iterators layout reads text boundaries with. */
final private[serenity] class BoundaryScratch:

  def characterBreaks: BreakIterator = BoundaryScratch.sharedCharacters.get()

  def lineBreaks: BreakIterator = BoundaryScratch.sharedLines.get()

private[serenity] object BoundaryScratch:

  private val sharedCharacters: ThreadLocal[BreakIterator] =
    ThreadLocal.withInitial(() => BreakIterator.getCharacterInstance())

  private val sharedLines: ThreadLocal[BreakIterator] =
    ThreadLocal.withInitial(() => BreakIterator.getLineInstance(Locale.ROOT))
