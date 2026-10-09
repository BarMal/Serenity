package com.serenity.rope

import com.serenity.testkit.ChangeSetGenerators.{applyToString, genChange, genRawIntents, genReplacement, genShortText}
import com.serenity.testkit.Generators
import org.scalacheck.{Gen, Shrink}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The laws [[ChangeSet]] must satisfy, each stated against the `String` the ropes were built from rather than against
  * the rope or change-set logic being checked. The mapping laws (L6, L7) live in `ChangeSetMappingParitySpec`, because
  * their oracle is the editor's legacy remapping code.
  */
class ChangeSetLawSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance                                     = Balance.default
  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 200)

  // A rope and the text it was built from shrink independently, which would report a pair that no longer agrees.
  given [A]: Shrink[A] = Shrink.shrinkAny

  extension [A](result: Option[A]) private def orFail: A = result.getOrElse(fail("expected the operation to succeed"))

  private val ropeTextAndChange: Gen[(Rope, String, ChangeSet)] =
    Generators.ropeWithText.flatMap {
      case (rope, text) => genChange(text.length).map(changes => (rope, text, changes))
    }

  private val ropeTextAndChain: Gen[(Rope, String, ChangeSet, ChangeSet, ChangeSet)] =
    for
      text  <- genShortText
      rope  <- Generators.ropeOfShape(text)
      first <- genChange(text.length)
      middle = applyToString(text, first)
      second <- genChange(middle.length)
      last   <- genChange(applyToString(middle, second).length)
    yield (rope, text, first, second, last)

  private def isCanonical(changes: ChangeSet): Boolean =
    changes.parts.forall(part =>
      !part.isNoOp && 0 <= part.from && part.from <= part.to && part.to <= changes.oldLength
    ) &&
      changes.parts.zip(changes.parts.drop(1)).forall((left, right) => left.to < right.from)

  /** The range a character-by-character comparison gives: the longest common prefix, then the longest common suffix of
    * what remains, as offsets in `afterText`.
    */
  private def exactRange(beforeText: String, afterText: String): Option[(Int, Int)] =
    val bound  = math.min(beforeText.length, afterText.length)
    val prefix = (0 until bound).find(i => beforeText(i) != afterText(i)).getOrElse(bound)
    if prefix == bound && beforeText.length == afterText.length then None
    else
      val suffixBound = bound - prefix
      val suffix =
        (0 until suffixBound)
          .find(i => beforeText(beforeText.length - 1 - i) != afterText(afterText.length - 1 - i))
          .getOrElse(suffixBound)
      Some((prefix, afterText.length - suffix))

  private def within(inner: (Int, Int), outer: (Int, Int)): Boolean = outer._1 <= inner._1 && inner._2 <= outer._2

  property("L1: any intents yield canonical form under either overlap policy") {
    forAll(Gen.choose(0, 30).flatMap(length => genRawIntents(length).map(intents => (length, intents)))) {
      case (length, intents) =>
        isCanonical(ChangeSet.fromIntents(length, intents, OverlapPolicy.ConcatAtPoint)) shouldBe true
        isCanonical(ChangeSet.fromIntents(length, intents, OverlapPolicy.MergeDeletions)) shouldBe true
    }
  }

  property("L1: canonicalising a canonical change set changes nothing") {
    forAll(Gen.choose(0, 30).flatMap(length => genChange(length).map(changes => (length, changes)))) {
      case (length, changes) =>
        ChangeSet.fromIntents(length, changes.parts, OverlapPolicy.ConcatAtPoint) shouldBe changes
    }
  }

  property("L1: single, compose, invert and fromDiff yield canonical form") {
    forAll(ropeTextAndChain) {
      case (rope, text, first, second, last) =>
        val composed = first.compose(second).orFail.compose(last).orFail
        val after    = composed.applyTo(rope).orFail
        isCanonical(composed) shouldBe true
        isCanonical(composed.invert(rope).orFail) shouldBe true
        isCanonical(ChangeSet.fromDiff(rope, after)) shouldBe true
        forAll(genReplacement(text.length)) { replacement =>
          isCanonical(ChangeSet.single(text.length, replacement.from, replacement.to, replacement.insert)) shouldBe true
        }
    }
  }

  property("L1: touching parts merge into one, concatenating their text, and no-op parts are dropped") {
    val touching = Seq(Replacement(2, 4, "a"), Replacement(4, 6, "b"), Replacement(9, 9, ""), Replacement(7, 7, ""))
    ChangeSet.fromIntents(10, touching, OverlapPolicy.ConcatAtPoint).parts shouldBe Vector(Replacement(2, 6, "ab"))
  }

  property("L1: intents outside the document are clamped to it") {
    ChangeSet
      .fromIntents(5, Seq(Replacement(-3, 2, "x"), Replacement(4, 99, "y")), OverlapPolicy.ConcatAtPoint)
      .parts shouldBe
      Vector(Replacement(0, 2, "x"), Replacement(4, 5, "y"))
  }

  property("L2: applyTo equals applying the parts to the String, whatever the shape of the rope") {
    forAll(ropeTextAndChange) {
      case (rope, text, changes) =>
        val expected = applyToString(text, changes)
        changes.applyTo(rope).map(_.collect()) shouldBe Some(expected)
        changes.newLength shouldBe expected.length
    }
  }

  property("L2: applyTo refuses a rope that is not the length the change set was made for") {
    forAll(ropeTextAndChange) {
      case (rope, _, changes) =>
        changes.applyTo(rope.concat(Rope("x"))) shouldBe None
    }
  }

  property("L3: the identity change set returns the very same rope instance") {
    forAll(Generators.ropeWithText) {
      case (rope, _) =>
        val result = ChangeSet.identity(rope.weight).applyTo(rope).orFail
        (result: AnyRef) shouldBe theSameInstanceAs(rope: AnyRef)
    }
  }

  property("L3: a change set built only from no-op edits is the identity and returns the same rope instance") {
    forAll(Generators.ropeWithText) {
      case (rope, _) =>
        val noOp = ChangeSet.single(rope.weight, rope.weight / 2, rope.weight / 2, "")
        noOp.isIdentity shouldBe true
        (noOp.applyTo(rope).orFail: AnyRef) shouldBe theSameInstanceAs(rope: AnyRef)
        ChangeSet.fromDiff(rope, rope).isIdentity shouldBe true
    }
  }

  property("L4: compose applies like applying one change set after the other") {
    forAll(ropeTextAndChain) {
      case (rope, text, first, second, _) =>
        val composed = first.compose(second).orFail
        composed.oldLength shouldBe first.oldLength
        composed.newLength shouldBe second.newLength
        composed.applyTo(rope).map(_.collect()) shouldBe Some(applyToString(applyToString(text, first), second))
    }
  }

  property("L4: compose is associative") {
    forAll(ropeTextAndChain) {
      case (_, _, first, second, last) =>
        first.compose(second).flatMap(_.compose(last)) shouldBe second.compose(last).flatMap(first.compose)
    }
  }

  property("L4: the identity change set is the unit of compose on both sides") {
    forAll(ropeTextAndChange) {
      case (_, _, changes) =>
        ChangeSet.identity(changes.oldLength).compose(changes) shouldBe Some(changes)
        changes.compose(ChangeSet.identity(changes.newLength)) shouldBe Some(changes)
    }
  }

  property("L4: compose refuses a change set that starts from a different length") {
    forAll(ropeTextAndChange) {
      case (_, _, changes) =>
        changes.compose(ChangeSet.identity(changes.newLength + 1)) shouldBe None
    }
  }

  property("L5: invert round-trips: the inverse restores the text, and inverting twice restores the change set") {
    forAll(ropeTextAndChange) {
      case (rope, text, changes) =>
        val after   = changes.applyTo(rope).orFail
        val inverse = changes.invert(rope).orFail
        inverse.oldLength shouldBe changes.newLength
        inverse.newLength shouldBe changes.oldLength
        inverse.applyTo(after).map(_.collect()) shouldBe Some(text)
        inverse.invert(after) shouldBe Some(changes)
    }
  }

  property("L5: invert refuses a rope that is not the length the change set was made for") {
    forAll(ropeTextAndChange) {
      case (rope, _, changes) =>
        changes.invert(rope.concat(Rope("x"))) shouldBe None
    }
  }

  property("L8: fromDiff covers the exact difference and is exactly the range RopeDiff reports") {
    forAll(ropeTextAndChange) {
      case (rope, text, changes) =>
        val after     = changes.applyTo(rope).orFail
        val afterText = applyToString(text, changes)
        val diff      = ChangeSet.fromDiff(rope, after)
        val ropeDiff  = RopeDiff.changedOffsetRange(rope, after)
        diff.applyTo(rope).map(_.collect()) shouldBe Some(afterText)
        diff.newEnvelope shouldBe ropeDiff
        exactRange(text, afterText).foreach(exact => diff.newEnvelope.exists(within(exact, _)) shouldBe true)
        ropeDiff.isEmpty shouldBe (text == afterText)
        diff.isIdentity shouldBe (text == afterText)
    }
  }

  property("L8: the envelope of any single edit is at least as wide as the exact difference it causes") {
    forAll(Generators.ropeWithText.flatMap {
      case (rope, text) =>
        genReplacement(text.length).map(replacement => (rope, text, replacement))
    }) {
      case (rope, text, replacement) =>
        val changes   = ChangeSet.single(text.length, replacement.from, replacement.to, replacement.insert)
        val afterText = applyToString(text, changes)
        changes.applyTo(rope).map(_.collect()) shouldBe Some(afterText)
        exactRange(text, afterText).foreach { exact =>
          changes.newEnvelope.exists(envelope => envelope._2 - envelope._1 >= exact._2 - exact._1) shouldBe true
        }
    }
  }

  property(
    "pinned L8 ambiguity: an inserted character equal to its neighbour has an exact difference beside its envelope"
  ) {
    val changes = ChangeSet.single(3, 1, 1, "a")
    changes.applyTo(Rope("aab")).map(_.collect()) shouldBe Some("aaab")
    exactRange("aab", "aaab") shouldBe Some((2, 3))
    changes.newEnvelope shouldBe Some((1, 2))
  }

  property("L8: single built from the exact difference lies between the exact difference and the RopeDiff range") {
    forAll(ropeTextAndChange) {
      case (rope, text, changes) =>
        val after     = changes.applyTo(rope).orFail
        val afterText = applyToString(text, changes)
        exactRange(text, afterText).foreach {
          case exact @ (start, end) =>
            val minimal = ChangeSet.single(
              text.length,
              start,
              text.length - (afterText.length - end),
              afterText.substring(start, end)
            )
            minimal.applyTo(rope).map(_.collect()) shouldBe Some(afterText)
            minimal.newEnvelope shouldBe Some(exact)
            RopeDiff.changedOffsetRange(rope, after).exists(within(minimal.newEnvelope.orFail, _)) shouldBe true
        }
    }
  }

  property("the envelopes bound every part, and text outside them is unchanged") {
    forAll(ropeTextAndChange) {
      case (_, text, changes) =>
        val afterText = applyToString(text, changes)
        (changes.oldEnvelope, changes.newEnvelope) match
          case (Some((oldStart, oldEnd)), Some((newStart, newEnd))) =>
            text.take(oldStart) shouldBe afterText.take(newStart)
            text.drop(oldEnd) shouldBe afterText.drop(newEnd)
          case other =>
            other shouldBe ((None, None))
            changes.isIdentity shouldBe true
    }
  }

  property("newRanges locate exactly the inserted text of each part in the new document") {
    forAll(ropeTextAndChange) {
      case (_, text, changes) =>
        val afterText = applyToString(text, changes)
        changes.newRanges.map((start, end) => afterText.substring(start, end)) shouldBe changes.parts.map(_.insert)
    }
  }

  property("lineSpans: every line outside a span is unchanged, shifted by the spans before it") {
    forAll(ropeTextAndChange) {
      case (rope, text, changes) =>
        val spans       = changes.lineSpans(rope)
        val beforeLines = text.split("\n", -1)
        val afterLines  = applyToString(text, changes).split("\n", -1)
        def shiftAt(line: Int): Int =
          spans.takeWhile(_.oldLast < line).lastOption.map(s => s.newLast - s.oldLast).getOrElse(0)
        beforeLines.indices.filterNot(line => spans.exists(s => s.oldFirst <= line && line <= s.oldLast)).foreach {
          line => afterLines(line + shiftAt(line)) shouldBe beforeLines(line)
        }
        spans.isEmpty shouldBe changes.isIdentity
        spans.zip(spans.drop(1)).foreach((left, right) => left.oldLast should be < right.oldFirst)
    }
  }

  property("lineSpans: an edit within one line spans only that line, and a newline widens the new side only") {
    val rope = Rope("one\ntwo\nthree")
    ChangeSet.single(13, 5, 5, "X").lineSpans(rope) shouldBe Vector(LineSpan(1, 1, 1, 1))
    ChangeSet.single(13, 5, 5, "X\nY").lineSpans(rope) shouldBe Vector(LineSpan(1, 1, 1, 2))
    ChangeSet.single(13, 2, 9, "").lineSpans(rope) shouldBe Vector(LineSpan(0, 2, 0, 0))
    ChangeSet
      .fromIntents(13, Seq(Replacement(0, 0, "a"), Replacement(12, 12, "b")), OverlapPolicy.ConcatAtPoint)
      .lineSpans(rope) shouldBe
      Vector(LineSpan(0, 0, 0, 0), LineSpan(2, 2, 2, 2))
  }
