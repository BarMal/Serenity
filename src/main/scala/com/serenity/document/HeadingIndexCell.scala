package com.serenity.document

import java.util.concurrent.atomic.AtomicReference

import com.serenity.rope.{ChangeSet, Rope}

/** The latest [[MarkdownHeadingIndex]] of one document's lineage, shared by every copy of that `Document` (#1848), so
  * the outline, chapter ghosts, notes pane and chapter renumbering read one parse per edit between them instead of each
  * parsing the text.
  *
  * An index is advanced to a text by the change the document recorded since the version that was last read, and by
  * diffing the two ropes when it recorded none (an undo, a reload), never by trusting that the last read was the
  * previous edit, so a copy of the document that skipped a read costs a wider window at worst and can never serve a
  * stale heading. It is a cache of derivable facts: every cell compares equal to every other, so a warm one and a cold
  * one do not make two otherwise-equal documents unequal.
  */
final class HeadingIndexCell:

  private val latest = new AtomicReference(Option.empty[HeadingIndexCell.Read])

  /** The headings of `content`, the text at `version`; `changeSince` is the document's record of what took a remembered
    * version and text to `content`.
    */
  def indexFor(content: Rope, version: Long, changeSince: (Long, Rope) => Option[ChangeSet]): MarkdownHeadingIndex =
    latest.get() match
      case Some(read) if read.index.content eq content => read.index
      case previous =>
        val index = previous.fold(MarkdownHeadingIndex.build(content))(read =>
          read.index.updatedTo(content, changeSince(read.version, read.index.content))
        )
        latest.set(Some(HeadingIndexCell.Read(version, index)))
        index

  override def equals(other: Any): Boolean =
    other match
      case _: HeadingIndexCell => true
      case _                   => false

  override def hashCode: Int = 0

  override def toString: String = "HeadingIndexCell"

object HeadingIndexCell:
  final private case class Read(version: Long, index: MarkdownHeadingIndex)
