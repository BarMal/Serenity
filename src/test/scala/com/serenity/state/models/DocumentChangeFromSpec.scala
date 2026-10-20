package com.serenity.state.models

import com.serenity.rope.{Balance, ChangeSet, Rope}
import com.serenity.testkit.UniqueTextEdits
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocumentChangeFromSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def document(text: String): Document = Document(Rope(text))

  "A document" should "report the change that took an earlier state of it to this one" in {
    val before = document("hello")
    val edited = before.edited(Rope("hello!"), ChangeSet.single(5, 5, 5, "!"))

    edited.changeFrom(before) shouldBe Some(ChangeSet.single(5, 5, 5, "!"))
    edited.contentVersion shouldBe before.contentVersion + 1
  }

  it should "compose the changes between two states several edits apart" in {
    val start  = document("abc")
    val first  = start.edited(Rope("abXc"), ChangeSet.single(3, 2, 2, "X"))
    val second = first.edited(Rope("abXcY"), ChangeSet.single(4, 4, 4, "Y"))

    second.changeFrom(start).flatMap(_.applyTo(start.content)).map(_.collect()) shouldBe Some("abXcY")
  }

  it should "not trust an edit whose lengths do not fit the texts, leaving a gap instead" in {
    val before = document("abc")
    val edited = before.edited(Rope("abcdef"), ChangeSet.single(3, 0, 0, "x"))

    edited.content.collect() shouldBe "abcdef"
    edited.contentVersion shouldBe before.contentVersion + 1
    edited.changeFrom(before) shouldBe None
  }

  it should "report no change for a state it only knows by comparing, such as a reload" in {
    val before = document("abc")

    before.withContent(Rope("abd")).changeFrom(before) shouldBe None
  }

  it should "report identity only for the very same text at the same version" in {
    val before = document("abc")

    before.changeFrom(before) shouldBe Some(ChangeSet.identity(3))
    before.copy(content = Rope("abc")).changeFrom(before) shouldBe None
  }

  "textDiffersFrom" should "agree with comparing the texts, edited or not" in
    UniqueTextEdits.cases(seed = 1838, count = 300).foreach { found =>
      val before = Document(found.before)
      val edited = before.edited(found.after, found.change)
      val reload = before.withContent(found.after)

      edited.textDiffersFrom(before) shouldBe (found.before != found.after)
      reload.textDiffersFrom(before) shouldBe (found.before != found.after)
      edited.textDiffersFrom(edited) shouldBe false
    }
