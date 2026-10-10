package com.serenity.document

import java.util.concurrent.atomic.AtomicReference

import com.serenity.rope.Rope

/** The latest [[MarkdownHeadingIndex]] of one document's lineage, shared by every copy of that `Document` (#1848), so
  * the outline, chapter ghosts, notes pane and chapter renumbering read one parse per edit between them instead of each
  * parsing the text.
  *
  * An index is advanced to a text by diffing the two ropes, never by trusting that the last read was the previous edit,
  * so an undo, a reload or a copy of the document that skipped a read costs a wider window at worst and can never serve
  * a stale heading. It is a cache of derivable facts: every cell compares equal to every other, so a warm one and a
  * cold one do not make two otherwise-equal documents unequal.
  */
final class HeadingIndexCell:

  private val latest = new AtomicReference(Option.empty[MarkdownHeadingIndex])

  def indexFor(content: Rope): MarkdownHeadingIndex =
    latest.get() match
      case Some(index) if index.content eq content => index
      case previous =>
        val index = previous.fold(MarkdownHeadingIndex.build(content))(_.updatedTo(content))
        latest.set(Some(index))
        index

  override def equals(other: Any): Boolean =
    other match
      case _: HeadingIndexCell => true
      case _                   => false

  override def hashCode: Int = 0

  override def toString: String = "HeadingIndexCell"
