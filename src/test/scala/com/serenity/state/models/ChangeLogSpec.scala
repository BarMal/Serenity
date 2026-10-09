package com.serenity.state.models

import com.serenity.rope.{Balance, ChangeSet, Rope}
import com.serenity.state.manager.TypedRuns
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChangeLogSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def document(text: String): Document = Document(Rope(text))

  private def appended(before: Document, at: Int, text: String): Document =
    before
      .applied(ChangeSet.single(before.content.weight, at, at, text))
      .getOrElse(fail("expected the change to apply"))

  private def typed(start: Document, keys: Int): Document =
    (0 until keys).foldLeft(start)((current, _) => appended(current, current.content.weight, "x"))

  private def changeSince(doc: Document, version: Long): ChangeSet =
    doc.changesSince(version).getOrElse(fail(s"expected a change since version $version"))

  "A document" should "apply a change set, advancing its version and its text" in {
    val edited = appended(document("abc"), 1, "XY")

    edited.content.collect() shouldBe "aXYbc"
    edited.contentVersion shouldBe 1L
    edited.isDirty shouldBe true
  }

  it should "refuse a change set made for another text" in {
    document("abc").applied(ChangeSet.single(10, 0, 0, "x")) shouldBe None
  }

  "since" should "return the change made at that version" in {
    val before = document("abc")
    val change = ChangeSet.single(3, 1, 2, "ZZ")
    val after  = before.applied(change).getOrElse(fail("expected the change to apply"))

    after.changesSince(before.contentVersion) shouldBe Some(change)
  }

  it should "be the identity at the current version" in {
    val edited = appended(document("abc"), 3, "d")

    changeSince(edited, edited.contentVersion).isIdentity shouldBe true
    changeSince(edited, edited.contentVersion).oldLength shouldBe 4
  }

  it should "compose the changes between two versions" in {
    val start  = document("hello")
    val second = appended(start, 5, " world")
    val third  = second.applied(ChangeSet.single(11, 0, 1, "J")).getOrElse(fail("expected the change to apply"))

    changeSince(third, 0).applyTo(start.content).map(_.collect()) shouldBe Some("Jello world")
    changeSince(third, 1).applyTo(second.content).map(_.collect()) shouldBe Some("Jello world")
  }

  it should "be unknown for a version the document has not reached" in {
    document("abc").changesSince(3L) shouldBe None
  }

  "withContent" should "leave a gap that no change can be composed across" in {
    val first  = appended(document("abc"), 3, "d")
    val gapped = first.withContent(Rope("something else"))
    val healed = appended(gapped, 0, "!")

    gapped.changesSince(first.contentVersion) shouldBe None
    healed.changesSince(0L) shouldBe None
    healed.changesSince(gapped.contentVersion) shouldBe Some(ChangeSet.single(14, 0, 0, "!"))
  }

  "replacedWith" should "record the region that differs" in {
    val before = document("the quick brown fox")
    val after  = before.replacedWith(Rope("the slow brown fox"))

    after.contentVersion shouldBe 1L
    changeSince(after, 0).applyTo(before.content).map(_.collect()) shouldBe Some("the slow brown fox")
  }

  it should "keep the rope it was given" in {
    val replacement = Rope("other")

    document("text").replacedWith(replacement).content should be theSameInstanceAs replacement
  }

  "A document whose version was set from outside the log" should "know no history" in {
    val moved = typed(document("a"), 3).copy(contentVersion = 40L)

    moved.changesSince(1L) shouldBe None
    moved.changesSince(40L).map(_.isIdentity) shouldBe Some(true)
  }

  "replacingContentOf" should "move past the previous version and start a fresh log" in {
    val previous = typed(document("a"), 5)
    val reloaded = document("fresh").replacingContentOf(previous)

    reloaded.contentVersion shouldBe previous.contentVersion + 1
    reloaded.changesSince(previous.contentVersion) shouldBe None
  }

  "The log's capacity" should "hold at least a full typed run and the edits around it" in {
    ChangeLog.Capacity should be >= TypedRuns.MaxKeys + 32
  }

  it should "keep a typed run composable end to end" in {
    val start = document("")
    val run   = typed(start, TypedRuns.MaxKeys + 32)

    changeSince(run, 0).applyTo(start.content).map(_.collect()) shouldBe Some("x" * (TypedRuns.MaxKeys + 32))
  }

  it should "evict the oldest entry first once it is full" in {
    val run    = typed(document(""), ChangeLog.Capacity + 10)
    val oldest = 10L

    run.changesSince(oldest - 1) shouldBe None
    run.changesSince(oldest).map(_.oldLength) shouldBe Some(10)
  }

  "The log's character cap" should "evict entries once their inserted text outgrows it" in {
    val small    = appended(document(""), 0, "a")
    val huge     = appended(small, 1, "b" * (ChangeLog.MaxInsertedChars + 1))
    val followup = appended(huge, huge.content.weight, "c")

    huge.changesSince(small.contentVersion) shouldBe None
    huge.changesSince(0L) shouldBe None
    followup.changesSince(huge.contentVersion).map(_.newLength) shouldBe Some(huge.content.weight + 1)
  }

  it should "keep entries whose text fits" in {
    val small = appended(document(""), 0, "a" * 1000)
    val more  = appended(small, 1000, "b" * 1000)

    more.changesSince(0L).map(_.newLength) shouldBe Some(2000)
  }

  "A change log" should "not distinguish documents that hold the same text at the same version" in {
    val edited = appended(document("abc"), 0, "x")

    edited shouldBe edited.copy(changes = ChangeLog.empty)
  }

end ChangeLogSpec
