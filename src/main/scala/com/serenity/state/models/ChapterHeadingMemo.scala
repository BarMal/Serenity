package com.serenity.state.models

import com.serenity.ui.layout.Symbol

/** The chapter headings of the buffer the notes pane last followed (#1848), so a cursor that changes line without an
  * edit finds its chapter without re-parsing the whole document.
  *
  * Every instance compares equal to every other, as [[BufferIndexMemos]] does: a warm cache and a cold one must not
  * make two otherwise-identical states unequal.
  */
final class ChapterHeadingMemo private (entry: Option[(BufferId, Memo[List[(HeadingIdentity, Symbol)]])]):

  def forBuffer(bufferId: BufferId): Option[Memo[List[(HeadingIdentity, Symbol)]]] =
    entry.collect { case (`bufferId`, memo) => memo }

  /** Holding `memo` for `bufferId` in place of whatever was held; `this` when it already is. */
  def holding(bufferId: BufferId, memo: Memo[List[(HeadingIdentity, Symbol)]]): ChapterHeadingMemo =
    if forBuffer(bufferId).exists(_ eq memo) then this else new ChapterHeadingMemo(Some(bufferId -> memo))

  override def equals(other: Any): Boolean =
    other match
      case _: ChapterHeadingMemo => true
      case _                     => false

  override def hashCode: Int = 0

  override def toString: String = "ChapterHeadingMemo"

object ChapterHeadingMemo:
  val empty: ChapterHeadingMemo = new ChapterHeadingMemo(None)
