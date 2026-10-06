package com.serenity.state.reducers

import com.serenity.rope.{Balance, ChangeSet, MapMode, OverlapPolicy, Replacement, Rope}
import com.serenity.state.models.{
  Buffer,
  BufferId,
  CursorPosition,
  DocumentComment,
  Placeholder,
  offsetToCursorPosition
}
import com.serenity.state.reducers.EditorEditSupport.MultiCursorEdit
import com.serenity.testkit.ChangeSetGenerators.{applyToString, genChange, genParts, genReplacement, genShortText}
import com.serenity.testkit.Generators
import org.scalacheck.{Gen, Shrink}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** [[ChangeSet]] position mapping and application against the editor code they are to replace, which stays in the tree
  * as the oracle: `EditorEditSupport.remapEditBoundary` (comment starts and ends, placeholders, bookmarks),
  * `remapOffsetAfterDeletions` and `mergeOverlappingDeletionRanges`, and the offset remap and content fold inside
  * `applyTrackedEdits`.
  *
  * Positions are compared at every offset of the document, not at sampled ones. Where the legacy code and a canonical
  * change set cannot agree, the disagreement is pinned by an example whose name says so, so the day the legacy code is
  * retired nobody mistakes it for something the new code got wrong.
  */
class ChangeSetMappingParitySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance                                     = Balance.default
  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 200)

  given [A]: Shrink[A] = Shrink.shrinkAny

  private val bufferId = BufferId(0)

  extension [A](result: Option[A]) private def orFail: A = result.getOrElse(fail("expected the operation to succeed"))

  private def legacyEdits(parts: Seq[Replacement]): List[MultiCursorEdit] =
    parts.zipWithIndex.map((part, index) => MultiCursorEdit(index, part.from, part.to, part.insert)).toList

  /** Edits separated by at least one unchanged character, the shape on which the legacy per-edit remap and a canonical
    * change set must agree.
    */
  private val separatedEdits: Gen[(String, List[Replacement])] =
    genShortText.flatMap(text => genParts(text.length, minGap = 1).map(parts => (text, parts)))

  /** Edits that may touch, and several insertions at one point, handed over in no particular order. */
  /** The legacy remaps return their input untouched when handed no edits, so there is nothing to compare for them. */
  private val separatedEditsThatCount: Gen[(String, List[Replacement])] = separatedEdits.suchThat(_._2.nonEmpty)

  private val shuffledEdits: Gen[(String, List[Replacement])] =
    genShortText.flatMap { text =>
      genParts(text.length, minGap = 0).flatMap { parts =>
        Gen.listOfN(parts.size, Gen.choose(0, 1000)).map(keys => (text, parts.zip(keys).sortBy(_._2).map(_._1)))
      }
    }

  private val genChain: Gen[(String, ChangeSet, ChangeSet)] =
    for
      text   <- genShortText
      first  <- genChange(text.length)
      second <- genChange(applyToString(text, first).length)
    yield (text, first, second)

  private def trackedCursors(buffer: Buffer, offsets: List[Int], parts: Seq[Replacement]): List[CursorPosition] =
    EditorEditSupport.applyTrackedEdits(buffer, offsets, legacyEdits(parts))._1.editing.cursorPositions

  private def legacyMapped(position: Int, parts: Seq[Replacement], mode: MapMode): Int =
    EditorEditSupport.remapEditBoundary(
      position,
      legacyEdits(parts),
      insertionAtBoundaryMoves = mode == MapMode.AnchorStart
    )

  property("L6: mapPos with AnchorStart equals remapEditBoundary for a comment start") {
    forAll(separatedEdits) {
      case (text, parts) =>
        val changes = ChangeSet.fromIntents(text.length, parts, OverlapPolicy.ConcatAtPoint)
        (0 to text.length).foreach { position =>
          changes.mapPos(position, MapMode.AnchorStart) shouldBe legacyMapped(position, parts, MapMode.AnchorStart)
        }
    }
  }

  property("L6: mapPos with AnchorEnd equals remapEditBoundary for a comment end") {
    forAll(separatedEdits) {
      case (text, parts) =>
        val changes = ChangeSet.fromIntents(text.length, parts, OverlapPolicy.ConcatAtPoint)
        (0 to text.length).foreach { position =>
          changes.mapPos(position, MapMode.AnchorEnd) shouldBe legacyMapped(position, parts, MapMode.AnchorEnd)
        }
    }
  }

  property("L6: Point is pinned to AnchorEnd, because placeholders and bookmarks use the comment-end rule today") {
    forAll(separatedEdits) {
      case (text, parts) =>
        val changes = ChangeSet.fromIntents(text.length, parts, OverlapPolicy.ConcatAtPoint)
        (0 to text.length).foreach { position =>
          changes.mapPos(position, MapMode.Point) shouldBe legacyMapped(position, parts, MapMode.Point)
          changes.mapPos(position, MapMode.Point) shouldBe changes.mapPos(position, MapMode.AnchorEnd)
        }
    }
  }

  property("L6: mapPos with Cursor equals the offset remap inside applyTrackedEdits") {
    forAll(separatedEditsThatCount) {
      case (text, parts) =>
        val changes = ChangeSet.fromIntents(text.length, parts, OverlapPolicy.ConcatAtPoint)
        val after   = changes.applyTo(Rope(text)).orFail
        (0 to text.length).foreach { position =>
          trackedCursors(Buffer.fromString(bufferId, text), List(position), parts) shouldBe
            List(after.offsetToCursorPosition(changes.mapPos(position, MapMode.Cursor)))
        }
    }
  }

  property("L6: mapSorted with Cursor equals the cursors applyTrackedEdits produces") {
    forAll(separatedEditsThatCount) {
      case (text, parts) =>
        val changes = ChangeSet.fromIntents(text.length, parts, OverlapPolicy.ConcatAtPoint)
        val after   = changes.applyTo(Rope(text)).orFail
        val offsets = (0 to text.length).toList
        val mapped  = changes.mapSorted(IArray.from(offsets), MapMode.Cursor).toList
        trackedCursors(Buffer.fromString(bufferId, text), offsets, parts) shouldBe
          mapped.map(after.offsetToCursorPosition).distinct
    }
  }

  property(
    "L6: mapSorted agrees with mapPos at every position, for every mode, with repeated and out-of-range positions"
  ) {
    forAll(shuffledEdits.flatMap {
      case (text, parts) =>
        Gen.listOf(Gen.choose(0, text.length + 3)).map(positions => (text, parts, positions.sorted))
    }) {
      case (text, parts, positions) =>
        val changes = ChangeSet.fromIntents(text.length, parts, OverlapPolicy.ConcatAtPoint)
        MapMode.values.foreach { mode =>
          changes.mapSorted(IArray.from(positions), mode).toList shouldBe positions.map(changes.mapPos(_, mode))
        }
    }
  }

  property("L6: the annotation remaps of comments, placeholders and bookmarks equal mapPos in the matching mode") {
    forAll(separatedEditsThatCount.flatMap {
      case (text, parts) =>
        Gen.listOfN(4, Gen.choose(0, text.length)).map(offsets => (text, parts, offsets))
    }) {
      case (text, parts, offsets) =>
        val before          = Rope(text)
        val changes         = ChangeSet.fromIntents(text.length, parts, OverlapPolicy.ConcatAtPoint)
        val after           = changes.applyTo(before).orFail
        val edits           = legacyEdits(parts)
        def at(offset: Int) = before.offsetToCursorPosition(offset)

        val placeholders = offsets.map(offset => Placeholder(at(offset), "note"))
        EditorEditSupport.adjustPlaceholders(placeholders, before, after, edits).map(_.position) shouldBe
          offsets.map(offset => after.offsetToCursorPosition(changes.mapPos(offset, MapMode.Point)))

        EditorEditSupport.adjustBookmarks(offsets.map(at), before, after, edits) shouldBe
          offsets.map(offset => after.offsetToCursorPosition(changes.mapPos(offset, MapMode.Point))).distinct

        val spans    = offsets.zip(offsets.drop(1)).map((a, b) => (math.min(a, b), math.max(a, b)))
        val comments = spans.map((start, end) => DocumentComment(at(start), at(end), "note"))
        EditorEditSupport.adjustDocumentComments(comments, before, after, edits).zip(spans).foreach {
          case (comment, (start, end)) =>
            val nextStart = changes.mapPos(start, MapMode.AnchorStart)
            val nextEnd   = changes.mapPos(end, MapMode.AnchorEnd).max(nextStart)
            (comment.anchor, comment.focus) shouldBe
              ((after.offsetToCursorPosition(nextStart), after.offsetToCursorPosition(nextEnd)))
        }
    }
  }

  property("L6: merged deletion ranges equal mergeOverlappingDeletionRanges, with touching ranges also joined") {
    forAll(genShortText.flatMap(text => genDeletions(text.length).map(ranges => (text, ranges)))) {
      case (text, ranges) =>
        val changes = ChangeSet.fromIntents(
          text.length,
          ranges.map((from, to) => Replacement(from, to, "")),
          OverlapPolicy.MergeDeletions
        )
        val legacy = EditorEditSupport.mergeOverlappingDeletionRanges(ranges).filter((from, to) => from < to)
        val joined = legacy
          .foldLeft(List.empty[(Int, Int)]) {
            case ((start, end) :: rest, (nextStart, nextEnd)) if nextStart <= end =>
              (start, math.max(end, nextEnd)) :: rest
            case (acc, range) => range :: acc
          }
          .reverse
        changes.parts.map(part => (part.from, part.to)).toList shouldBe joined
    }
  }

  property("L6: with one merged deletion range, mapPos equals remapOffsetAfterDeletions in every mode") {
    forAll(genShortText.flatMap(text => genWindowedDeletions(text.length).map(ranges => (text, ranges)))) {
      case (text, ranges) =>
        val merged = EditorEditSupport.mergeOverlappingDeletionRanges(ranges)
        val changes = ChangeSet.fromIntents(
          text.length,
          ranges.map((from, to) => Replacement(from, to, "")),
          OverlapPolicy.MergeDeletions
        )
        if merged.size <= 1 then
          (0 to text.length).foreach { position =>
            MapMode.values.foreach { mode =>
              changes.mapPos(position, mode) shouldBe EditorEditSupport.remapOffsetAfterDeletions(position, merged)
            }
          }
    }
  }

  property(
    "pinned legacy quirk: remapOffsetAfterDeletions mixes shifted offsets with unshifted ranges after two deletions"
  ) {
    val deletions = List((2, 4), (6, 8))
    val changes =
      ChangeSet.fromIntents(12, deletions.map((from, to) => Replacement(from, to, "")), OverlapPolicy.MergeDeletions)
    EditorEditSupport.remapOffsetAfterDeletions(9, deletions) shouldBe 6
    changes.mapPos(9, MapMode.Cursor) shouldBe 5
  }

  property(
    "pinned legacy quirk: edits that touch are remapped one by one, a change set maps against their merged region"
  ) {
    val parts   = List(Replacement(4, 4, "x"), Replacement(4, 6, "y"))
    val changes = ChangeSet.fromIntents(10, parts, OverlapPolicy.ConcatAtPoint)
    changes.parts shouldBe Vector(Replacement(4, 6, "xy"))
    legacyMapped(4, parts, MapMode.AnchorEnd) shouldBe 5
    changes.mapPos(4, MapMode.AnchorEnd) shouldBe 4

    val cursorParts   = List(Replacement(2, 4, ""), Replacement(4, 4, "y"))
    val cursorChanges = ChangeSet.fromIntents(6, cursorParts, OverlapPolicy.ConcatAtPoint)
    trackedCursors(Buffer.fromString(bufferId, "abcdef"), List(3), cursorParts) shouldBe List(CursorPosition(0, 2))
    cursorChanges.mapPos(3, MapMode.Cursor) shouldBe 3
  }

  property(
    "pinned legacy order: insertions at one point are concatenated last-intent-first, as sequential inserts do"
  ) {
    val parts = List(Replacement(2, 2, "a"), Replacement(2, 2, "b"), Replacement(2, 2, "c"))
    ChangeSet
      .fromIntents(4, parts, OverlapPolicy.ConcatAtPoint)
      .applyTo(Rope("wxyz"))
      .orFail
      .collect() shouldBe "wxcbayz"
    EditorEditSupport
      .applyTrackedEdits(Buffer.fromString(bufferId, "wxyz"), List(0), legacyEdits(parts))
      ._1
      .document
      .content
      .collect() shouldBe "wxcbayz"
  }

  property("applyTo matches applyTrackedEdits on content, for touching edits and same-point insertions in any order") {
    forAll(shuffledEdits) {
      case (text, parts) =>
        val changes = ChangeSet.fromIntents(text.length, parts, OverlapPolicy.ConcatAtPoint)
        val legacy = EditorEditSupport.applyTrackedEdits(Buffer.fromString(bufferId, text), List(0), legacyEdits(parts))
        changes.applyTo(Rope(text)).map(_.collect()) shouldBe Some(legacy._1.document.content.collect())
    }
  }

  property("applyTo matches applyMergedDeletionEdits on content, for overlapping and touching deletions") {
    forAll(genShortText.flatMap(text => genDeletions(text.length).map(ranges => (text, ranges)))) {
      case (text, ranges) =>
        val changes = ChangeSet.fromIntents(
          text.length,
          ranges.map((from, to) => Replacement(from, to, "")),
          OverlapPolicy.MergeDeletions
        )
        val edits  = ranges.map((from, to) => MultiCursorEdit(0, from, to, ""))
        val legacy = EditorEditSupport.applyMergedDeletionEdits(Buffer.fromString(bufferId, text), List(0), edits)
        changes.applyTo(Rope(text)).map(_.collect()) shouldBe Some(legacy._1.document.content.collect())
    }
  }

  property("MergeDeletions ignores inserted text, as applyMergedDeletionEdits does") {
    val changes =
      ChangeSet.fromIntents(6, List(Replacement(1, 3, "zzz"), Replacement(2, 5, "q")), OverlapPolicy.MergeDeletions)
    changes.parts shouldBe Vector(Replacement(1, 5, ""))
  }

  property("k = 1: applyTo builds the same rope, leaf for leaf, as today's single insert or delete") {
    forAll(Generators.ropeWithText.flatMap {
      case (rope, text) =>
        genReplacement(text.length).map(replacement => (rope, text, replacement))
    }) {
      case (rope, text, edit) =>
        val changes = ChangeSet.single(text.length, edit.from, edit.to, edit.insert)
        val legacy =
          if edit.insert.isEmpty then EditorEditSupport.deleteOrUnchanged(rope, edit.from, edit.to)
          else if edit.from == edit.to then EditorEditSupport.insertOrUnchanged(rope, edit.from, edit.insert)
          else
            EditorEditSupport.insertOrUnchanged(
              EditorEditSupport.deleteOrUnchanged(rope, edit.from, edit.to),
              edit.from,
              edit.insert
            )
        changes.applyTo(rope).orFail shouldBe legacy
    }
  }

  property("k = 1: the same holds on a large rope with many leaves") {
    val text = (1 to 3000).map(line => s"line $line of the document").mkString("\n")
    val rope = Rope(text)
    forAll(Gen.choose(0, text.length), Gen.choose(0, 40), Gen.oneOf("", "x", "typed\nline")) {
      (from, removed, insert) =>
        val to = math.min(text.length, from + removed)
        whenever(!(from == to && insert.isEmpty)) {
          val legacy =
            if insert.isEmpty then EditorEditSupport.deleteOrUnchanged(rope, from, to)
            else if from == to then EditorEditSupport.insertOrUnchanged(rope, from, insert)
            else EditorEditSupport.insertOrUnchanged(EditorEditSupport.deleteOrUnchanged(rope, from, to), from, insert)
          ChangeSet.single(text.length, from, to, insert).applyTo(rope).orFail shouldBe legacy
        }
    }
  }

  property("L7: a position outside every composed part maps the same through the composition as through each in turn") {
    forAll(genChain) {
      case (text, first, second) =>
        val composed = first.compose(second).orFail
        val untouched =
          (0 to text.length).filter(position =>
            composed.parts.forall(part => position < part.from || position > part.to)
          )
        untouched.foreach { position =>
          MapMode.values.foreach { mode =>
            composed.mapPos(position, mode) shouldBe second.mapPos(first.mapPos(position, mode), mode)
          }
        }
    }
  }

  property("pinned L7 divergence: Cursor on the start of a region that composition grows collapses to the grown end") {
    val first    = ChangeSet.single(12, 8, 9, "j")
    val second   = ChangeSet.single(12, 7, 8, "")
    val composed = first.compose(second).orFail
    composed.parts shouldBe Vector(Replacement(7, 9, "j"))
    second.mapPos(first.mapPos(7, MapMode.Cursor), MapMode.Cursor) shouldBe 7
    composed.mapPos(7, MapMode.Cursor) shouldBe 8
  }

  property("pinned L7 divergence: AnchorEnd mapped in turn stops before text a later change inserts at the boundary") {
    val first    = ChangeSet.single(10, 2, 3, "WNyG")
    val second   = ChangeSet.single(13, 6, 6, "9ER")
    val composed = first.compose(second).orFail
    composed.parts shouldBe Vector(Replacement(2, 3, "WNyG9ER"))
    second.mapPos(first.mapPos(3, MapMode.AnchorEnd), MapMode.AnchorEnd) shouldBe 6
    composed.mapPos(3, MapMode.AnchorEnd) shouldBe 9
  }

  property("pinned L7 divergence: AnchorStart mapped in turn moves past text a later change inserts at the boundary") {
    val first    = ChangeSet.single(6, 2, 4, "")
    val second   = ChangeSet.single(4, 2, 2, "x")
    val composed = first.compose(second).orFail
    composed.parts shouldBe Vector(Replacement(2, 4, "x"))
    second.mapPos(first.mapPos(3, MapMode.AnchorStart), MapMode.AnchorStart) shouldBe 3
    composed.mapPos(3, MapMode.AnchorStart) shouldBe 2
  }

  private def genDeletions(length: Int): Gen[List[(Int, Int)]] =
    val genRange =
      for
        a <- Gen.choose(0, length)
        b <- Gen.choose(0, length)
      yield (math.min(a, b), math.max(a, b))
    Gen.choose(0, 5).flatMap(count => Gen.listOfN(count, genRange))

  /** Ranges inside a window of seven characters, so most of them overlap into a single merged range. */
  private def genWindowedDeletions(length: Int): Gen[List[(Int, Int)]] =
    val window = math.min(length, 6)
    val genRange =
      for
        start <- Gen.choose(0, window)
        end   <- Gen.choose(start, window)
      yield (start, end)
    Gen.choose(1, 3).flatMap(count => Gen.listOfN(count, genRange))
