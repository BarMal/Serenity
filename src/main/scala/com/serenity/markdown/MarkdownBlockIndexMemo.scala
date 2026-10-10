package com.serenity.markdown

import java.util.concurrent.atomic.AtomicReference

import com.serenity.markdown.MarkdownBlockLens.FenceRangeIndex
import com.serenity.rope.Rope

/** The block index of a document, kept for the few most recently indexed ropes. A rope is immutable and every edit
  * makes a new one, so an index found by rope identity is current for exactly as long as the content is unchanged --
  * which lets code with no render cache at hand, such as caret movement, ask for it on every key press.
  *
  * It has no parameters so that every memo is equal to every other: a state that carries one still compares equal to
  * the same state with another, since what the memo holds never changes what the state means.
  */
final case class MarkdownBlockIndexMemo():

  private val recent = AtomicReference(Vector.empty[(Rope, FenceRangeIndex)])

  def of(content: Rope): FenceRangeIndex =
    recent.get.find(entry => sameRope(entry._1, content)).map(_._2).getOrElse(indexAndRemember(content))

  private def sameRope(a: Rope, b: Rope): Boolean = (a: AnyRef) eq (b: AnyRef)

  private def indexAndRemember(content: Rope): FenceRangeIndex =
    val found = MarkdownBlockLens.fenceRangeIndex(content.linesIteratorFrom(0).map(_._2))
    val _     = recent.updateAndGet(entries => ((content, found) +: entries).take(MarkdownBlockIndexMemo.MaxEntries))
    found

object MarkdownBlockIndexMemo:

  private val MaxEntries = 8
