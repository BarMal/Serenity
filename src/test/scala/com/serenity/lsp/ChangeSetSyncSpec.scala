package com.serenity.lsp

import com.serenity.lsp.client.{DocumentUri, LspProtocol}
import com.serenity.lsp.model.{LspPosition, TextChangeDiff, TextDocumentSyncKind}
import com.serenity.rope.{Balance, ChangeSet, Rope}
import com.serenity.testkit.ChangeSetGenerators.{applyToString, genChange, genShortText}
import com.serenity.testkit.UniqueTextEdits
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The content changes sent to a server for a known change set (#1838): applied one after another, as a server does,
  * they turn the old text into the new one, and for a single edit they are what comparing the two texts reports.
  */
class ChangeSetSyncSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance = Balance.default

  private def offsetOf(text: String, position: LspPosition): Int =
    text.split("\n", -1).take(position.line).map(_.length + 1).sum + position.character

  private def appliedInOrder(text: String, changes: List[TextChangeDiff.Change]): String =
    changes.foldLeft(text) { (current, change) =>
      val from = offsetOf(current, change.range.start)
      val to   = offsetOf(current, change.range.end)
      change.rangeLength shouldBe (to - from)
      current.substring(0, from) + change.text + current.substring(to)
    }

  private val genEdited: Gen[(String, ChangeSet)] =
    genShortText.flatMap(text => genChange(text.length).map(text -> _))

  property("content changes for a multi-part change set, applied in order, make the new text") {
    forAll(genEdited) { (text, change) =>
      appliedInOrder(text, TextChangeDiff.changes(Rope(text), change)) shouldBe applyToString(text, change)
    }
  }

  property("a change set with no parts sends no content changes") {
    forAll(genShortText)(text => TextChangeDiff.changes(Rope(text), ChangeSet.identity(text.length)) shouldBe Nil)
  }

  property("one edit sends exactly the change comparing the texts reports") {
    UniqueTextEdits.cases(seed = 1838, count = 300).foreach { found =>
      TextChangeDiff.changes(found.before, found.change) shouldBe List(TextChangeDiff.diff(found.before, found.after))
    }
  }

  property("incremental didChange carries one content change per part, last first") {
    forAll(genEdited) { (text, change) =>
      val params = LspProtocol.didChangeParams(
        DocumentUri("file:///a.scala"),
        3,
        Rope(text),
        Rope(applyToString(text, change)),
        change,
        TextDocumentSyncKind.Incremental
      )
      val changes = params.hcursor.downField("contentChanges").values.toList.flatten
      changes.size shouldBe change.parts.size
      changes.flatMap(_.hcursor.get[String]("text").toOption) shouldBe change.parts.reverse.map(_.insert)
    }
  }

  property("full didChange sends the new text whatever the change set") {
    forAll(genEdited) { (text, change) =>
      val after = applyToString(text, change)
      val params = LspProtocol.didChangeParams(
        DocumentUri("file:///a.scala"),
        3,
        Rope(text),
        Rope(after),
        change,
        TextDocumentSyncKind.Full
      )
      params.hcursor
        .downField("contentChanges")
        .values
        .toList
        .flatten
        .flatMap(_.hcursor.get[String]("text").toOption) shouldBe List(after)
    }
  }

  property("a delta joins the next only when that starts where it ends, and applies only to the text it starts from") {
    val first  = DocumentDelta(ChangeSet.single(3, 3, 3, "d"), fromVersion = 4L, toVersion = 5L)
    val second = DocumentDelta(ChangeSet.single(4, 0, 0, "x"), fromVersion = 5L, toVersion = 6L)

    first.andThen(second).map(_.change.applyTo(Rope("abc")).map(_.collect())) shouldBe Some(Some("xabcd"))
    first.andThen(second).map(joined => (joined.fromVersion, joined.toVersion)) shouldBe Some((4L, 6L))
    second.andThen(first) shouldBe None
    first.appliesTo(Some(4L), Rope("abc"), Rope("abcd")) shouldBe true
    first.appliesTo(Some(3L), Rope("abc"), Rope("abcd")) shouldBe false
    first.appliesTo(None, Rope("abc"), Rope("abcd")) shouldBe false
    first.appliesTo(Some(4L), Rope("abcd"), Rope("abcde")) shouldBe false
  }
