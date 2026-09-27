package com.serenity.richtext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParagraphTreeSpec extends AnyFlatSpec with Matchers:

  private def paragraph(text: String): RichTextParagraph = RichTextParagraph.plain(text)

  private def linear(count: Int): List[RichTextParagraph] =
    (0 until count).map(index => paragraph(s"line-$index")).toList

  "ParagraphTree" should "report zero paragraphs and characters when built from an empty list" in {
    val tree = ParagraphTree.fromParagraphs(Nil)

    tree.paragraphCount shouldBe 0
    tree.charCount shouldBe 0
    tree.toParagraphs shouldBe Nil
    tree.paragraphAt(0) shouldBe None
  }

  it should "round-trip a single paragraph" in {
    val tree = ParagraphTree.fromParagraphs(List(paragraph("only")))

    tree.paragraphCount shouldBe 1
    tree.charCount shouldBe 4
    tree.toParagraphs shouldBe List(paragraph("only"))
    tree.paragraphAt(0) shouldBe Some(paragraph("only"))
    tree.paragraphAt(1) shouldBe None
    tree.paragraphAt(-1) shouldBe None
  }

  it should "round-trip a very large document, preserving order" in {
    val paragraphs = linear(20_000)
    val tree        = ParagraphTree.fromParagraphs(paragraphs)

    tree.paragraphCount shouldBe 20_000
    tree.toParagraphs shouldBe paragraphs
    tree.paragraphAt(0) shouldBe Some(paragraph("line-0"))
    tree.paragraphAt(9_999) shouldBe Some(paragraph("line-9999"))
    tree.paragraphAt(19_999) shouldBe Some(paragraph("line-19999"))
    tree.paragraphAt(20_000) shouldBe None
  }

  it should "split at the very start, the very end, and the middle" in {
    val tree = ParagraphTree.fromParagraphs(linear(10))

    val (atStartLeft, atStartRight) = ParagraphTree.splitAt(tree, 0)
    atStartLeft.toParagraphs shouldBe Nil
    atStartRight.toParagraphs shouldBe linear(10)

    val (atEndLeft, atEndRight) = ParagraphTree.splitAt(tree, 10)
    atEndLeft.toParagraphs shouldBe linear(10)
    atEndRight.toParagraphs shouldBe Nil

    val (midLeft, midRight) = ParagraphTree.splitAt(tree, 4)
    midLeft.toParagraphs shouldBe linear(10).take(4)
    midRight.toParagraphs shouldBe linear(10).drop(4)
  }

  it should "split out of range by clamping to the nearest end" in {
    val tree = ParagraphTree.fromParagraphs(linear(5))

    val (negLeft, negRight) = ParagraphTree.splitAt(tree, -3)
    negLeft.toParagraphs shouldBe Nil
    negRight.toParagraphs shouldBe linear(5)

    val (bigLeft, bigRight) = ParagraphTree.splitAt(tree, 999)
    bigLeft.toParagraphs shouldBe linear(5)
    bigRight.toParagraphs shouldBe Nil
  }

  it should "link two trees back together in order" in {
    val (left, right) = ParagraphTree.splitAt(ParagraphTree.fromParagraphs(linear(2_000)), 733)

    ParagraphTree.link(left, right).toParagraphs shouldBe linear(2_000)
  }

  it should "treat linking with an empty tree as an identity" in {
    val tree  = ParagraphTree.fromParagraphs(linear(7))
    val empty = ParagraphTree.fromParagraphs(Nil)

    ParagraphTree.link(tree, empty).toParagraphs shouldBe linear(7)
    ParagraphTree.link(empty, tree).toParagraphs shouldBe linear(7)
  }

  it should "transform only the paragraphs inside the requested index range" in {
    val tree = ParagraphTree.fromParagraphs(linear(10))

    val updated = tree.updatedRange(3, 5) { (paragraph, index) =>
      paragraph.copy(runs = List(RichTextRun(s"touched-$index")))
    }

    updated.toParagraphs.map(_.plainText) shouldBe List(
      "line-0",
      "line-1",
      "line-2",
      "touched-3",
      "touched-4",
      "touched-5",
      "line-6",
      "line-7",
      "line-8",
      "line-9"
    )
  }

  it should "reuse untouched subtrees by reference when a range update misses them" in {
    val tree = ParagraphTree.fromParagraphs(linear(10_000))

    val updated = tree.updatedRange(1, 1)((paragraph, _) => paragraph.normalized)

    (tree, updated) match
      case (ParagraphTree.Branch(leftBefore, _, _, _), ParagraphTree.Branch(leftAfter, _, _, _)) =>
        (leftBefore eq leftAfter) shouldBe false // the touched paragraph lives under the left half
      case _ => fail("expected the 10,000-paragraph tree to be a branch")

    (tree, updated) match
      case (ParagraphTree.Branch(_, rightBefore, _, _), ParagraphTree.Branch(_, rightAfter, _, _)) =>
        (rightBefore eq rightAfter) shouldBe true // untouched half must be shared, not rebuilt
      case _ => fail("expected the 10,000-paragraph tree to be a branch")
  }

  it should "leave every paragraph untouched when the range matches nothing" in {
    val tree    = ParagraphTree.fromParagraphs(linear(5))
    val updated = tree.updatedRange(50, 60)((paragraph, _) => paragraph.normalized)

    (tree eq updated) shouldBe true
  }

  it should "replace a contiguous slice with a differently-sized replacement" in {
    val tree = ParagraphTree.fromParagraphs(linear(6))

    val replaced = tree.replaceSlice(2, 3, List(paragraph("a"), paragraph("b"), paragraph("c")))

    replaced.toParagraphs.map(_.plainText) shouldBe List("line-0", "line-1", "a", "b", "c", "line-4", "line-5")
  }

  it should "replace the entire document when the slice spans it" in {
    val tree     = ParagraphTree.fromParagraphs(linear(4))
    val replaced = tree.replaceSlice(0, 3, List(paragraph("only")))

    replaced.toParagraphs shouldBe List(paragraph("only"))
  }

  it should "map every paragraph while preserving shape" in {
    val tree    = ParagraphTree.fromParagraphs(linear(500))
    val mapped  = tree.mapAll(p => p.copy(alignment = ParagraphAlignment.Center))

    mapped.paragraphCount shouldBe 500
    mapped.toParagraphs.forall(_.alignment == ParagraphAlignment.Center) shouldBe true
  }

  it should "short circuit existsAny once a match is found" in {
    val tree = ParagraphTree.fromParagraphs(
      List(paragraph("plain"), paragraph("plain"), RichTextParagraph.plain("styled", role = ParagraphRole.Heading(1)))
    )

    tree.existsAny(_.role != ParagraphRole.Body) shouldBe true
    tree.existsAny(_.role == ParagraphRole.Heading(2)) shouldBe false
  }

  it should "evaluate forallInRange only over paragraphs inside the range" in {
    val tree = ParagraphTree.fromParagraphs(
      List(
        RichTextParagraph.plain("out", role = ParagraphRole.Heading(1)),
        paragraph("in-range-a"),
        paragraph("in-range-b"),
        RichTextParagraph.plain("also-out", role = ParagraphRole.Heading(2))
      )
    )

    val inRangeResult  = tree.forallInRange(1, 2)((paragraph, _) => paragraph.role == ParagraphRole.Body)
    val outOfRangeResult = tree.forallInRange(0, 2)((paragraph, _) => paragraph.role == ParagraphRole.Body)
    inRangeResult shouldBe true
    outOfRangeResult shouldBe false
  }

  it should "treat an out-of-range forallInRange as vacuously true" in {
    val tree   = ParagraphTree.fromParagraphs(linear(5))
    val result = tree.forallInRange(50, 60)((_, _) => false)
    result shouldBe true
  }
