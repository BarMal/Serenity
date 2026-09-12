package com.serenity.state.reducers

import com.serenity.rope.{Balance, Leaf, Rope}

// `Rope` is sealed, so a test double can no longer extend it directly; it delegates to a real `Leaf`/`Node` tree
// while itself extending the still-open `Leaf` purely to satisfy the type system -- every method that matters for
// these tests forwards to `delegate` rather than using anything inherited from `Leaf`.
//
// Shared across the "without materialising the whole buffer" test groups in this package (document comment
// tracking, and the broader non-materialising-operations coverage) so the same double isn't redefined per file.
final class NonCollectingRope(delegate: Rope)(using Balance) extends Leaf(delegate.collect()):
  override def weight: Int =
    delegate.weight

  override def height: Int =
    delegate.height

  override val newlineCount: Int =
    delegate.newlineCount

  override val lastLineLength: Int =
    delegate.lastLineLength

  override val endsWithNewline: Boolean =
    delegate.endsWithNewline

  override def isWeightBalanced: Boolean =
    delegate.isWeightBalanced

  override def isHeightBalanced: Boolean =
    delegate.isHeightBalanced

  override def rebalance: Rope =
    this

  override def index(i: Int): Option[Char] =
    delegate.index(i)

  override def splitAt(index: Int): Option[(Rope, Rope)] =
    delegate.splitAt(index)

  override def lineCount: Int =
    delegate.lineCount

  override def getLine(lineIndex: Int): Option[String] =
    delegate.getLine(lineIndex)

  override def lineColumnToOffset(line: Int, column: Int): Int =
    delegate.lineColumnToOffset(line, column)

  override def collect(): String =
    throw AssertionError("navigation should not materialise the whole buffer")

object NonCollectingRope:
  def apply(delegate: Rope)(using Balance): NonCollectingRope = new NonCollectingRope(delegate)
