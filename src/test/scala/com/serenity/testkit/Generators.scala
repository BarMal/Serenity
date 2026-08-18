package com.serenity.testkit

import com.serenity.rope.{Balance, Leaf, Node, Rope}
import com.serenity.state.models.{BufferId, CursorPosition, PaneId, Selection}
import org.scalacheck.{Arbitrary, Cogen, Gen}

/** Generators shared across property and law suites.
  *
  * The rope generators exist to satisfy the representation-invariance requirement in `docs/coding-standards.md`: rope
  * behaviour should be tested independently of tree shape. Hand-written fixtures can only ever exercise the shapes
  * someone thought to write down, so [[ropeOfShape]] builds arbitrary trees over a given string and
  * [[differentlyShapedRopes]] pairs two of them over identical content.
  */
object Generators:

  given Balance = Balance.default

  /** Text without carriage returns.
    *
    * `Rope.apply` normalises CRLF and bare CR to LF on entry, so a generator that emitted them would produce ropes
    * whose content differs from the string they were built from, and every comparison property would fail for a reason
    * that has nothing to do with the rope.
    */
  val genText: Gen[String] =
    Gen.listOf(Gen.frequency(20 -> Gen.alphaNumChar, 3 -> Gen.const(' '), 2 -> Gen.const('\n'))).map(_.mkString)

  /** Text likely to contain several lines, for line-addressing properties. */
  val genMultilineText: Gen[String] =
    Gen.listOf(Gen.listOf(Gen.alphaNumChar).map(_.mkString)).map(_.mkString("\n"))

  /** An arbitrarily shaped rope whose content is exactly `text`.
    *
    * Splits at arbitrary points and never rebalances, so the resulting trees are deliberately lopsided in ways
    * `Rope.apply` would never produce. Depth is bounded so generation terminates on long strings.
    */
  def ropeOfShape(text: String, maxDepth: Int = 6): Gen[Rope] =
    if text.isEmpty || maxDepth <= 0 then Gen.const(Leaf(text))
    else
      Gen.frequency(
        1 -> Gen.const(Leaf(text)),
        3 -> Gen
          .chooseNum(0, text.length)
          .flatMap { splitAt =>
            for
              left  <- ropeOfShape(text.take(splitAt), maxDepth - 1)
              right <- ropeOfShape(text.drop(splitAt), maxDepth - 1)
            yield Node(left, right)
          }
      )

  /** Two ropes holding identical content in different tree shapes, with that content. */
  val differentlyShapedRopes: Gen[(Rope, Rope, String)] =
    for
      text  <- genText
      left  <- ropeOfShape(text)
      right <- ropeOfShape(text)
    yield (left, right, text)

  /** A rope paired with the string it was built from, so properties can state the expected answer in terms of `String`
    * rather than restating rope logic.
    */
  val ropeWithText: Gen[(Rope, String)] =
    for
      text <- genText
      rope <- ropeOfShape(text)
    yield (rope, text)

  given Arbitrary[Rope] = Arbitrary(ropeWithText.map(_._1))

  given Arbitrary[BufferId] = Arbitrary(Gen.chooseNum(0, 1000).map(BufferId.apply))
  given Cogen[BufferId]     = Cogen[Int].contramap(_.value)

  given Arbitrary[PaneId] = Arbitrary(Gen.chooseNum(0, 1000).map(PaneId.apply))
  given Cogen[PaneId]     = Cogen[Int].contramap(_.value)

  val genCursorPosition: Gen[CursorPosition] =
    for
      line   <- Gen.chooseNum(0, 500)
      column <- Gen.chooseNum(0, 500)
    yield CursorPosition(line, column)

  given Arbitrary[CursorPosition] = Arbitrary(genCursorPosition)
  given Cogen[CursorPosition]     = Cogen[(Int, Int)].contramap(cursor => (cursor.line, cursor.column))

  given Arbitrary[Selection] =
    Arbitrary(
      for
        anchor <- genCursorPosition
        focus  <- genCursorPosition
      yield Selection(anchor, focus)
    )
