package com.serenity.ui.layout

import com.serenity.markdown.{LazyBlockIndex, MarkdownInlineView, MarkerMode}
import com.serenity.rope.{Balance, Rope}
import com.serenity.ui.layout.VisualLineIndexStore.{StampRelation, ViewStamp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ViewStampRelationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val lines = Vector.tabulate(1_500)(line => s"# Heading $line")

  private def ropeOf(texts: Vector[String]): Rope = Rope(texts.mkString("\n"))

  private def stampOf(index: LazyBlockIndex, revealed: Vector[Range.Inclusive] = Vector.empty): ViewStamp =
    ViewStamp(VisualLineIndexStore.Unstamped, MarkdownInlineView(MarkerMode.Live, revealed, index))

  "Row-count stamps over a block index" should "stand for the same counts while the index is the same one" in {
    val index = LazyBlockIndex.of(ropeOf(lines))

    VisualLineIndexStore.stampsRelate(stampOf(index), stampOf(index)) shouldBe StampRelation.Same
  }

  it should "keep every line above an edit's resume point when the index was resumed from the earlier one" in {
    val before = LazyBlockIndex.of(ropeOf(lines))
    before.rangeAt(1_200)
    val after = before.after(ropeOf(lines.updated(1_100, "edited")), 1_100)

    VisualLineIndexStore.stampsRelate(stampOf(before), stampOf(after)) shouldBe StampRelation.AgreesBefore(1_024)
  }

  it should "keep fewer lines for an edit nearer the top" in {
    val before = LazyBlockIndex.of(ropeOf(lines))
    before.rangeAt(1_200)
    val after = before.after(ropeOf(lines.updated(300, "edited")), 300)

    VisualLineIndexStore.stampsRelate(stampOf(before), stampOf(after)) shouldBe StampRelation.AgreesBefore(256)
  }

  it should "not relate indexes of unrelated texts" in {
    val first  = LazyBlockIndex.of(ropeOf(lines))
    val second = LazyBlockIndex.of(ropeOf(lines))

    VisualLineIndexStore.stampsRelate(stampOf(first), stampOf(second)) shouldBe StampRelation.Unrelated
  }

  it should "not relate views that restyle differently, whatever their indexes" in {
    val before = LazyBlockIndex.of(ropeOf(lines))
    val after  = before.after(ropeOf(lines.updated(1_100, "edited")), 1_100)

    VisualLineIndexStore.stampsRelate(stampOf(before), stampOf(after, Vector(3 to 3))) shouldBe StampRelation.Unrelated
  }

  "Unmeasuring from a line" should "drop the measurements from that line down and keep those above" in {
    val measured = (0 until 10).foldLeft(VisualLineIndex.unmeasured(10))((index, line) => index.measured(line, 2))

    val unmeasured = VisualLineIndexStore.unmeasuredFrom(measured, 6)

    (0 until 6).map(unmeasured.measuredRows) shouldBe Vector.fill(6)(Some(2))
    (6 until 10).map(unmeasured.measuredRows) shouldBe Vector.fill(4)(None)
  }
end ViewStampRelationSpec
