package com.serenity.testkit

import com.serenity.rope.{ChangeSet, OverlapPolicy, Replacement}
import org.scalacheck.Gen

/** Generators for [[ChangeSet]] laws, built on the text generators in [[Generators]]. */
object ChangeSetGenerators:

  private val genInsertedChar: Gen[Char] = Gen.frequency(8 -> Gen.alphaNumChar, 1 -> Gen.const('\n'))

  private val genNonEmptyText: Gen[String] =
    Gen.choose(1, 4).flatMap(size => Gen.listOfN(size, genInsertedChar).map(_.mkString))

  private val genInserted: Gen[String] = Gen.frequency(1 -> Gen.const(""), 3 -> genNonEmptyText)

  /** Document text short enough for the laws to compare every position. */
  val genShortText: Gen[String] = Gen.resize(40, Generators.genText)

  /** Valid, ascending, non-overlapping replacements for a document of `length`. Each starts at least `minGap` after the
    * previous one ends, so `minGap = 0` also yields touching parts and several insertions at one point.
    */
  def genParts(length: Int, minGap: Int): Gen[List[Replacement]] =
    def from(position: Int): Gen[List[Replacement]] =
      if position > length then Gen.const(Nil)
      else Gen.frequency(1 -> Gen.const(Nil), 5 -> part(position))

    def part(position: Int): Gen[List[Replacement]] =
      for
        start   <- Gen.choose(position, math.min(length, position + 6))
        removed <- Gen.choose(0, math.min(3, length - start))
        text    <- if removed == 0 then genNonEmptyText else genInserted
        rest    <- from(start + removed + minGap)
      yield Replacement(start, start + removed, text) :: rest

    from(0)

  /** Edits as separate cursors would make them: arbitrary offsets, in no particular order, overlapping freely and
    * sometimes outside the document.
    */
  def genRawIntents(length: Int): Gen[List[Replacement]] =
    val genIntent =
      for
        from <- Gen.choose(-2, length + 3)
        to   <- Gen.choose(-2, length + 3)
        text <- genInserted
      yield Replacement(from, to, text)
    Gen.choose(0, 6).flatMap(count => Gen.listOfN(count, genIntent))

  def genReplacement(length: Int): Gen[Replacement] =
    for
      start   <- Gen.choose(0, length)
      removed <- Gen.choose(0, math.min(3, length - start))
      text    <- if removed == 0 then genNonEmptyText else genInserted
    yield Replacement(start, start + removed, text)

  def genChange(length: Int): Gen[ChangeSet] =
    genParts(length, minGap = 1).map(ChangeSet.fromIntents(length, _, OverlapPolicy.ConcatAtPoint))

  /** `text` after `changes`, computed on the `String` alone so a law never restates the rope logic it checks. */
  def applyToString(text: String, changes: ChangeSet): String =
    val (edited, end) = changes.parts.foldLeft(("", 0)) {
      case ((acc, at), part) => (acc + text.substring(at, part.from) + part.insert, part.to)
    }
    edited + text.substring(end)
