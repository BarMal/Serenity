package com.serenity.ui.layout

import java.util.Locale

import com.ibm.icu.text.BreakIterator

/** The break iterators layout reads text boundaries with, kept by whoever owns the wraps (see [[LayoutMemos]]). An
  * iterator is stateful and not thread-safe, and building one per use measurably slowed wrapping (#1277), so each
  * thread that lays out text keeps one of each, built when it first asks and reused after. Two owners share none.
  */
final private[serenity] class BoundaryScratch:

  private val characters: ThreadLocal[BreakIterator] =
    ThreadLocal.withInitial(() => BreakIterator.getCharacterInstance())

  private val lines: ThreadLocal[BreakIterator] =
    ThreadLocal.withInitial(() => BreakIterator.getLineInstance(Locale.ROOT))

  def characterBreaks: BreakIterator = characters.get()

  def lineBreaks: BreakIterator = lines.get()
