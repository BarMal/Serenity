package com.serenity.lsp.model

import com.serenity.rope.{Balance, Rope}
import com.serenity.testkit.Generators
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The rope diff must produce exactly the change the text diff does for the same pair of documents: `didChange`
  * payloads are what servers see, and moving the computation onto ropes (so no full text is built per edit) is not
  * allowed to change them -- including for edits that touch surrogate pairs, which the text diff keeps together.
  */
class TextChangeDiffRopeSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance = Balance(weightBalance = 1000, heightBalance = 5, leafChunkSize = 8)

  // Written as chars: a lone surrogate cannot appear in a source literal.
  private val highSurrogate = 0xd83d.toChar.toString
  private val lowSurrogate  = 0xde00.toChar.toString

  private val genToken: Gen[String] =
    Gen.frequency(
      12 -> Gen.oneOf("a", "b", "c"),
      3  -> Gen.const("\n"),
      1  -> Gen.const(" "),
      2  -> Gen.oneOf("😀", "😁"),
      1  -> Gen.oneOf(highSurrogate, lowSurrogate)
    )

  private val genDocument: Gen[String] =
    Gen.choose(0, 120).flatMap(size => Gen.listOfN(size, genToken)).map(_.mkString)

  final private case class Edit(start: Int, deleted: Int, inserted: String)

  private def genEdit(text: String): Gen[Edit] =
    for
      start    <- Gen.choose(0, text.length)
      deleted  <- Gen.choose(0, text.length - start)
      inserted <- Gen.choose(0, 6).flatMap(size => Gen.listOfN(size, genToken)).map(_.mkString)
    yield Edit(start, deleted, inserted)

  private def applied(text: String, edit: Edit): String =
    text.take(edit.start) + edit.inserted + text.drop(edit.start + edit.deleted)

  private def appliedToRope(rope: Rope, edit: Edit): Rope =
    val removed = if edit.deleted == 0 then rope else rope.delete(edit.start, edit.start + edit.deleted).getOrElse(rope)
    if edit.inserted.isEmpty then removed else removed.insert(edit.start, edit.inserted).getOrElse(removed)

  property("an edit applied to the rope diffs exactly as the same edit diffs on the text") {
    forAll(genDocument.flatMap(text => genEdit(text).map(text -> _)), minSuccessful(500)) { (text, edit) =>
      val before = Rope(text)
      val after  = appliedToRope(before, edit)
      TextChangeDiff.diff(before, after) shouldBe TextChangeDiff.diff(text, applied(text, edit))
    }
  }

  property("two documents built independently diff exactly as their texts do") {
    forAll(genDocument, genDocument, minSuccessful(500)) { (oldText, newText) =>
      TextChangeDiff.diff(Rope(oldText), Rope(newText)) shouldBe TextChangeDiff.diff(oldText, newText)
    }
  }

  property("trees of arbitrary shape diff exactly as their texts do") {
    val pair =
      for
        oldText <- genDocument
        edit    <- genEdit(oldText)
        before  <- Generators.ropeOfShape(oldText)
        after   <- Generators.ropeOfShape(applied(oldText, edit))
      yield (oldText, applied(oldText, edit), before, after)
    forAll(pair, minSuccessful(500)) { (oldText, newText, before, after) =>
      TextChangeDiff.diff(before, after) shouldBe TextChangeDiff.diff(oldText, newText)
    }
  }

  property("a document diffed against itself is the same no-op the text diff reports") {
    forAll(genDocument) { text =>
      val rope = Rope(text)
      TextChangeDiff.diff(rope, rope) shouldBe TextChangeDiff.diff(text, text)
    }
  }

  property("an edit in a document with several lines is positioned on the right line and column") {
    val lines = Gen.choose(1, 40).flatMap(count => Gen.listOfN(count, Gen.alphaStr)).map(_.mkString("\n"))
    forAll(lines.flatMap(text => genEdit(text).map(text -> _)), minSuccessful(300)) { (text, edit) =>
      val before = Rope(text)
      TextChangeDiff.diff(before, appliedToRope(before, edit)) shouldBe TextChangeDiff.diff(text, applied(text, edit))
    }
  }

  property("an append at the end of the document is a zero-length insert") {
    val change = TextChangeDiff.diff(Rope("object Foo"), Rope("object Foo2"))
    change.range shouldBe LspRange(LspPosition(0, 10), LspPosition(0, 10))
    change.rangeLength shouldBe 0
    change.text shouldBe "2"
  }

  property("a surrogate pair stays together when only its second half changed") {
    val change = TextChangeDiff.diff(Rope("a\uD83D\uDE00b"), Rope("a\uD83D\uDE01b"))
    change.range shouldBe LspRange(LspPosition(0, 1), LspPosition(0, 3))
    change.text shouldBe "\uD83D\uDE01"
  }
