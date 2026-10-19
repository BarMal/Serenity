package com.serenity.ui.openings

import com.serenity.richtext.{
  DocumentFeature,
  InlineMark,
  ParagraphAlignment,
  ParagraphRole,
  RichTextDocument,
  RichTextParagraph,
  RichTextRun,
  RichTextStyle
}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RichTextOpeningsSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 1000)

  private def title(text: String)   = RichTextParagraph.plain(text, role = ParagraphRole.Heading(1))
  private def sub(text: String)     = RichTextParagraph.plain(text, role = ParagraphRole.Heading(2))
  private def para(text: String)    = RichTextParagraph.plain(text)
  private def blank                 = RichTextParagraph(Nil)
  private def centred(text: String) = RichTextParagraph.plain(text, alignment = ParagraphAlignment.Center)
  private def right(text: String)   = RichTextParagraph.plain(text, alignment = ParagraphAlignment.Right)
  private def dropCap(text: String) = RichTextParagraph.plain(text, role = ParagraphRole.dropCap())
  private def italic(text: String) =
    RichTextParagraph(List(RichTextRun(text, RichTextStyle().withMark(InlineMark.Italic))))
  private def block = RichTextParagraph.block("<w:tbl/>", DocumentFeature.Tables)

  private def document(paragraphs: RichTextParagraph*) = RichTextDocument(paragraphs.toList)

  private def lines(rules: OpeningRules, paragraphs: RichTextParagraph*): Vector[Opening] =
    linesMarking(_ => false)(rules, paragraphs*)

  private def linesMarking(explicit: Int => Boolean)(rules: OpeningRules, paragraphs: RichTextParagraph*) =
    ChapterOpenings.openingLines(document(paragraphs*), rules, explicit).openings

  private val default = OpeningRules.default

  "A chapter opening" should "be the first body paragraph after a chapter title" in {
    lines(default, title("One"), para("Opening."), para("Next.")) shouldBe
      Vector(Opening(1, OpeningKind.Chapter, handDropCap = false))
  }

  it should "skip empty paragraphs" in {
    lines(default, title("One"), blank, para("  "), para("Opening.")).map(_.line) shouldBe Vector(3)
  }

  it should "skip lower headings" in {
    lines(default, title("One"), sub("Part"), blank, sub("Subpart"), para("Opening.")).map(_.line) shouldBe Vector(4)
  }

  it should "be found after each of several titles" in {
    lines(default, title("One"), para("A."), para("B."), title("Two"), para("C.")).map(_.line) shouldBe Vector(1, 4)
  }

  it should "not exist when another title comes first" in {
    lines(default, title("One"), title("Two"), para("Opening.")).map(_.line) shouldBe Vector(2)
    lines(default, title("One"), blank, title("Two")) shouldBe empty
  }

  it should "not exist for a title at the end of the document" in {
    lines(default, para("Intro."), title("One")) shouldBe empty
  }

  it should "not follow an empty title" in {
    lines(default, para("Intro."), title(""), para("Next.")) shouldBe empty
  }

  it should "not exist without a chapter title" in {
    lines(default, para("Intro."), para("More."), sub("Part"), para("Again.")) shouldBe empty
  }

  it should "never be a one-character paragraph, which also ends the search" in {
    lines(default, title("One"), para("x"), para("Opening.")) shouldBe empty
  }

  it should "never be an opaque block, which also ends the search" in {
    lines(default, title("One"), block, para("Opening.")) shouldBe empty
  }

  it should "report a hand-made drop cap" in {
    lines(default, title("One"), dropCap("Opening.")) shouldBe
      Vector(Opening(1, OpeningKind.Chapter, handDropCap = true))
  }

  "The epigraph rule AutoDetect" should "skip centred, right-aligned and entirely italic paragraphs" in {
    lines(default, title("One"), centred("To the sea"), para("Opening.")).map(_.line) shouldBe Vector(2)
    lines(default, title("One"), right("-- Anon"), para("Opening.")).map(_.line) shouldBe Vector(2)
    lines(default, title("One"), italic("Call me."), blank, para("Opening.")).map(_.line) shouldBe Vector(3)
    lines(default, title("One"), centred("A"), right("B"), italic("C"), para("Opening.")).map(_.line) shouldBe
      Vector(4)
  }

  it should "not skip a paragraph that is only partly italic" in {
    val partly = RichTextParagraph(List(RichTextRun("Some ", RichTextStyle()), RichTextRun("words", italicStyle)))
    lines(default, title("One"), partly, para("Next.")).map(_.line) shouldBe Vector(1)
  }

  it should "leave nothing when only epigraphs follow the title" in {
    lines(default, title("One"), centred("To the sea")) shouldBe empty
  }

  private def italicStyle = RichTextStyle().withMark(InlineMark.Italic)

  "The epigraph rule Never" should "skip nothing" in {
    val never = default.copy(epigraph = EpigraphRule.Never)
    lines(never, title("One"), centred("To the sea"), para("Opening.")).map(_.line) shouldBe Vector(1)
    lines(never, title("One"), italic("Call me."), para("Opening.")).map(_.line) shouldBe Vector(1)
  }

  "The epigraph rule ExplicitOnly" should "skip only the paragraphs the caller marks" in {
    val explicitOnly = default.copy(epigraph = EpigraphRule.ExplicitOnly)
    linesMarking(_ == 2)(explicitOnly, title("One"), centred("Not marked"), para("Marked."), para("Opening."))
      .map(_.line) shouldBe
      Vector(1)
    linesMarking(_ == 2)(explicitOnly, title("One"), blank, para("Marked."), para("Opening."))
      .map(_.line) shouldBe Vector(3)
  }

  it should "skip nothing while no paragraph is marked" in {
    val explicitOnly = default.copy(epigraph = EpigraphRule.ExplicitOnly)
    lines(explicitOnly, title("One"), centred("To the sea"), para("Opening.")).map(_.line) shouldBe Vector(1)
  }

  it should "not read the marker under the other rules" in {
    linesMarking(_ => true)(default, title("One"), para("Opening.")).map(_.line) shouldBe Vector(1)
  }

  private def withSceneBreaks(rule: SceneBreakRule) = default.copy(sceneBreak = rule)

  "A scene break" should "give no opening under NoOpening" in {
    lines(withSceneBreaks(SceneBreakRule.NoOpening), para("Before."), para("***"), para("After.")) shouldBe empty
  }

  it should "give the paragraph after it a small-caps opening under SmallCapsOnly" in {
    lines(withSceneBreaks(SceneBreakRule.SmallCapsOnly), para("Before."), para("* * *"), blank, para("After.")) shouldBe
      Vector(Opening(3, OpeningKind.SceneBreakSmallCaps, handDropCap = false))
  }

  it should "give the paragraph after it a full opening under FullOpening" in {
    lines(withSceneBreaks(SceneBreakRule.FullOpening), para("Before."), para("⁂"), para("After.")) shouldBe
      Vector(Opening(2, OpeningKind.SceneBreakFull, handDropCap = false))
  }

  it should "end a chapter's search for its opening without being one" in {
    lines(withSceneBreaks(SceneBreakRule.NoOpening), title("One"), para("***"), para("After.")) shouldBe empty
    lines(withSceneBreaks(SceneBreakRule.FullOpening), title("One"), para("***"), para("After.")) shouldBe
      Vector(Opening(2, OpeningKind.SceneBreakFull, handDropCap = false))
  }

  it should "give an opening to the paragraph after each of several breaks" in {
    lines(withSceneBreaks(SceneBreakRule.FullOpening), para("A."), para("***"), para("B."), para("---"), para("C."))
      .map(_.line) shouldBe Vector(2, 4)
  }

  it should "not skip epigraphs, lower headings or one-character paragraphs" in {
    val full = withSceneBreaks(SceneBreakRule.FullOpening)
    lines(full, para("***"), centred("Aside"), para("After.")) shouldBe empty
    lines(full, para("***"), sub("Part"), para("After.")) shouldBe empty
    lines(full, para("***"), para("x"), para("After.")) shouldBe empty
  }

  it should "skip empty paragraphs and report a hand-made drop cap" in {
    lines(withSceneBreaks(SceneBreakRule.FullOpening), para("***"), blank, dropCap("After.")) shouldBe
      Vector(Opening(2, OpeningKind.SceneBreakFull, handDropCap = true))
  }

  it should "give a title right after a scene break its own opening" in {
    lines(withSceneBreaks(SceneBreakRule.FullOpening), para("***"), title("One"), para("Opening.")) shouldBe
      Vector(Opening(2, OpeningKind.Chapter, handDropCap = false))
  }

  "Opening lines" should "answer by line" in {
    val found = ChapterOpenings.openingLines(
      document(title("One"), para("A."), title("Two"), para("B.")),
      default,
      ChapterOpenings.noExplicitEpigraphs
    )
    found.lines shouldBe Set(1, 3)
    found.isOpening(1) shouldBe true
    found.at(3).map(_.kind) shouldBe Some(OpeningKind.Chapter)
    found.isOpening(0) shouldBe false
    found.isOpening(2) shouldBe false
    found.isOpening(99) shouldBe false
  }

  "The hand drop cap rule" should "be carried in the rules without changing what is detected" in
    HandDropCapRule.values.foreach { rule =>
      val found = lines(default.copy(handDropCap = rule), title("One"), dropCap("Opening."))
      found shouldBe Vector(Opening(1, OpeningKind.Chapter, handDropCap = true))
    }

  "The default rules" should "auto-detect epigraphs, give scene breaks no opening and let a hand drop cap win" in {
    OpeningRules.default shouldBe
      OpeningRules(EpigraphRule.AutoDetect, SceneBreakRule.NoOpening, HandDropCapRule.HandWins)
  }

  private val genRules: Gen[OpeningRules] =
    for
      epigraph   <- Gen.oneOf(EpigraphRule.values.toSeq)
      sceneBreak <- Gen.oneOf(SceneBreakRule.values.toSeq)
      handDrop   <- Gen.oneOf(HandDropCapRule.values.toSeq)
    yield OpeningRules(epigraph, sceneBreak, handDrop)

  private val genParagraph: Gen[RichTextParagraph] = Gen.frequency(
    4 -> Gen.oneOf("Body text.", "Another one.", "Third.").map(para),
    2 -> Gen.oneOf("One", "Two").map(title),
    1 -> Gen.const(sub("Part")),
    3 -> Gen.const(blank),
    1 -> Gen.oneOf("***", "---", "⁂").map(para),
    1 -> Gen.const(centred("Epigraph")),
    1 -> Gen.const(italic("Quoted")),
    1 -> Gen.const(para("x")),
    1 -> Gen.const(dropCap("Capped.")),
    1 -> Gen.const(block)
  )

  private val genParagraphs: Gen[List[RichTextParagraph]] = Gen.listOfN(40, genParagraph)

  private def explicitEvery(step: Int): Int => Boolean = _ % step == 0

  private def markedQuotes(doc: RichTextDocument): Int => Boolean =
    line => doc.paragraphAt(line).exists(_.plainText == "Quoted")

  "Opening detection" should "only ever open a body paragraph after something that can govern it" in
    forAll(genRules, genParagraphs) { (rules, paragraphs) =>
      val doc   = RichTextDocument(paragraphs)
      val found = ChapterOpenings.openingLines(doc, rules, explicitEvery(3))
      found.openings.foreach { opening =>
        val paragraph = paragraphs(opening.line)
        paragraph.plainText.trim.length should be >= 2
        paragraph.isOpaqueBlock shouldBe false
        paragraph.role.isInstanceOf[ParagraphRole.Heading] shouldBe false
        SceneBreaks.isSceneBreak(paragraph.plainText.trim) shouldBe false
        opening.handDropCap shouldBe paragraph.role.isInstanceOf[ParagraphRole.DropCap]
        opening.kind == OpeningKind.Chapter || rules.sceneBreak != SceneBreakRule.NoOpening shouldBe true
      }
      found.openings.map(_.line) shouldBe found.openings.map(_.line).distinct.sorted
    }

  it should "give a chapter title at most one opening, before the next title" in
    forAll(genRules, genParagraphs) { (rules, paragraphs) =>
      val found = ChapterOpenings.openingLines(RichTextDocument(paragraphs), rules, ChapterOpenings.noExplicitEpigraphs)
      val titles          = paragraphs.indices.filter(index => isTitle(paragraphs(index)))
      val chapterOpenings = found.openings.filter(_.kind == OpeningKind.Chapter).map(_.line)
      titles.zip(titles.drop(1).map(Some(_)).appended(None)).foreach { (start, next) =>
        chapterOpenings.count(line => line > start && next.forall(line < _)) should be <= 1
      }
    }

  private def isTitle(paragraph: RichTextParagraph): Boolean =
    paragraph.role == ParagraphRole.Heading(1) && paragraph.plainText.trim.nonEmpty

  it should "agree with a full rescan after any edit" in
    forAll(genRules, genParagraphs, genParagraphs, Gen.choose(0, 40), Gen.choose(0, 6), Gen.choose(0, 6)) {
      (rules, original, replacement, at, removed, inserted) =>
        val before   = RichTextDocument(original)
        val previous = ChapterOpenings.openingLines(before, rules, markedQuotes(before))
        val start    = at.min(original.size)
        val removing = original.slice(start, start + removed).length
        val added    = replacement.take(inserted)
        val after    = RichTextDocument(original.patch(start, added, removing))
        val edit     = LineEdit(start, removing, added.length)
        val updated  = ChapterOpenings.updatedOpeningLines(previous, after, edit, rules, markedQuotes(after))
        updated shouldBe ChapterOpenings.openingLines(after, rules, markedQuotes(after))
    }

  it should "rescan from scratch when the rules differ from those the earlier result used" in {
    val doc      = document(title("One"), para("Opening."), para("***"), para("After."))
    val previous = ChapterOpenings.openingLines(doc, default, ChapterOpenings.noExplicitEpigraphs)
    val full     = withSceneBreaks(SceneBreakRule.FullOpening)
    val updated =
      ChapterOpenings.updatedOpeningLines(previous, doc, LineEdit(0, 0, 0), full, ChapterOpenings.noExplicitEpigraphs)
    updated.openings.map(_.line) shouldBe Vector(1, 3)
  }
