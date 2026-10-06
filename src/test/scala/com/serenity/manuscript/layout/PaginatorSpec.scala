package com.serenity.manuscript.layout

import com.serenity.manuscript.layout.PaginatorFixture.*
import com.serenity.manuscript.typography.{FaceStyle, FontFamily, LineAlignment, MeasureError}
import com.serenity.manuscript.{Block, FrontMatter, ParagraphKind, Section, SectionHeading}
import com.serenity.richtext.{InlineMark, RichTextRun, RichTextStyle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, OptionValues}

class PaginatorSpec extends AnyFlatSpec with Matchers with EitherValues with OptionValues:

  private val Margin = 72f

  private def bodyLines(doc: PagedDocument): Vector[PlacedLine] = doc.pages.flatMap(_.lines).drop(1)

  "line breaking" should "break greedily at spaces so no line is wider than the 468-point measure" in {
    val text = Vector.fill(60)("lorem").mkString(" ")
    val doc  = paginate(manuscript(Vector(chapter("Chapter", prose(text)))))

    val lines = bodyLines(doc)
    lines.size should be > 1
    lines.foreach(line => visibleRight(line) should be <= Margin + 468f + 0.01f)
    lines.map(_.text).mkString shouldBe text
  }

  it should "fill each line: 72 characters on the indented first line, 78 after" in {
    val text = Vector.fill(40)("abcde").mkString(" ")
    val doc  = paginate(manuscript(Vector(chapter("H", prose(text)))))

    bodyLines(doc).map(_.text.stripTrailing.length) shouldBe Vector(71, 77, 77, 11)
  }

  it should "break a word wider than the measure at a code point rather than overflow" in {
    val doc = paginate(manuscript(Vector(chapter("H", prose("x" * 200)))))

    val lines = bodyLines(doc)
    lines.map(_.text.length) shouldBe Vector(72, 78, 50)
    lines.foreach(line => visibleRight(line) should be <= Margin + 468f + 0.01f)
  }

  "first-line indent and leading" should "indent only the first line of a paragraph, by half an inch" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 3)))))

    bodyLines(doc).flatMap(_.runs.headOption).map(_.x) shouldBe Vector(108f, 72f, 72f)
  }

  it should "space baselines one double-spaced pitch (24 points) apart" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 4)))))

    val baselines = bodyLines(doc).map(_.baselineY)
    baselines.zip(baselines.drop(1)).map((a, b) => b - a) shouldBe Vector(pitch, pitch, pitch)
  }

  it should "leave one blank line between a heading and the first paragraph" in {
    val lines = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 1))))).pages.flatMap(_.lines)

    lines.map(_.baselineY) shouldBe Vector(Margin + chapterDrop + pitch - 2f, Margin + chapterDrop + 3 * pitch - 2f)
  }

  it should "add the paragraph gap between paragraphs when the typography has one" in {
    val spaced = typography.copy(paragraphGap = 10f)
    val lines = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 1), paragraphOf('b', 1)))), spaced).pages
      .flatMap(_.lines)

    (lines(2).baselineY - lines(1).baselineY) shouldBe pitch + 10f
  }

  "chapters" should "each start a new page, dropped a third of the text height from the top margin" in {
    val doc = paginate(manuscript(Vector(chapter("One", paragraphOf('a', 2)), chapter("Two", paragraphOf('b', 2)))))

    doc.pages.map(_.kind) shouldBe Vector(PageKind.SectionStart, PageKind.SectionStart)
    doc.pages.flatMap(_.lines.headOption).map(_.baselineY) shouldBe Vector.fill(2)(Margin + chapterDrop + pitch - 2f)
    doc.pages.flatMap(_.lines.headOption).map(_.text) shouldBe Vector("One", "Two")
  }

  it should "centre every line of a multi-line heading" in {
    val heading = Section.Chapter(Some(SectionHeading(Vector("Chapter One", "Arrival"))), Vector(paragraphOf('a', 1)))
    val lines   = paginate(manuscript(Vector(heading))).pages.flatMap(_.lines)

    lines.take(2).map(_.text) shouldBe Vector("Chapter One", "Arrival")
    lines.take(2).flatMap(_.runs.headOption).map(_.x) shouldBe Vector(306f - 33f, 306f - 21f)
  }

  it should "start a part on its own page, and its first chapter on the next" in {
    val part = Section.Part(Some(SectionHeading.of("Part One")), Vector(chapter("Chapter", paragraphOf('a', 1))))
    val doc  = paginate(manuscript(Vector(part)))

    doc.pages.map(marks) shouldBe Vector("P", "Ca")
  }

  it should "not open a blank page when the first chapter is also the first page" in {
    paginate(manuscript(Vector(chapter("One", paragraphOf('a', 1))))).pages should have size 1
  }

  it should "flow a long chapter onto continuation pages that begin at the top margin" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 60)))))

    doc.pages.map(_.kind) shouldBe Vector(PageKind.SectionStart, PageKind.Body, PageKind.Body)
    doc.pages(1).lines.headOption.map(_.baselineY) shouldBe Some(Margin + pitch - 2f)
    doc.pages(1).lines should have size 27
  }

  "a scene break" should "be a centred glyph on a line of its own" in {
    val doc   = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 1), Block.SceneBreak, paragraphOf('b', 1)))))
    val lines = doc.pages.flatMap(_.lines)

    lines.map(_.text.take(1)) shouldBe Vector("H", "a", "#", "b")
    lines(2).runs.map(_.x) shouldBe Vector(306f - 3f)
  }

  it should "never end a page: it moves to the next page with the text it introduces" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 15), Block.SceneBreak, paragraphOf('b', 5)))))

    doc.pages.map(marks) shouldBe Vector("H" + "a" * 15, "#bbbbb")
  }

  it should "stay on the page when the text it introduces also starts there" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 5), Block.SceneBreak, paragraphOf('b', 5)))))

    doc.pages should have size 1
  }

  "keeping a heading with its next line" should "not strand a heading when the drop leaves no room for text" in {
    val deep = typography.copy(chapterDrop = 600.0 / 648.0)
    val doc  = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 3)))), deep)

    doc.pages.map(marks) shouldBe Vector("Haaa")
    doc.pages.flatMap(_.lines.headOption).map(_.baselineY) shouldBe Vector(Margin + pitch - 2f)
  }

  "widows and orphans" should "move a paragraph whole when fewer than two of its lines would start the page's end" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 15), paragraphOf('b', 4)))))

    doc.pages.map(marks) shouldBe Vector("H" + "a" * 15, "bbbb")
  }

  it should "pull a line over to the next page so that at least two lines are not left behind" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 12), paragraphOf('b', 5)))))

    doc.pages.map(marks) shouldBe Vector("H" + "a" * 12 + "bbb", "bb")
  }

  it should "honour larger limits from the typography" in {
    val strict = typography.copy(widows = 3, orphans = 3)
    val doc    = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 12), paragraphOf('b', 6)))), strict)

    doc.pages.map(marks) shouldBe Vector("H" + "a" * 12 + "bbb", "bbb")
  }

  it should "never split a paragraph shorter than orphans plus widows" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 14), paragraphOf('b', 3)))))

    doc.pages.map(marks) shouldBe Vector("H" + "a" * 14, "bbb")
  }

  it should "still make progress on a paragraph taller than a page" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 100)))))

    doc.pages.flatMap(_.lines).count(_.text.startsWith("a")) shouldBe 100
    doc.pages.lastOption.map(_.lines.size).value should be >= 2
  }

  "running heads" should "print surname, keyword and page number, right-aligned above the text, on body pages" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 60)))))

    val head = doc.pages(1).head.value
    head.text shouldBe "Writer / NIGHT / 2"
    head.runs.map(_.x) shouldBe Vector(Margin + 468f - 18 * 6f)
    head.baselineY shouldBe 36f + 10f
    doc.pages(2).head.map(_.text) shouldBe Some("Writer / NIGHT / 3")
  }

  it should "be suppressed on chapter-first pages" in {
    val doc = paginate(manuscript(Vector(chapter("One", paragraphOf('a', 40)), chapter("Two", paragraphOf('b', 2)))))

    doc.pages.filter(_.kind == PageKind.SectionStart).map(_.head) shouldBe Vector(None, None)
  }

  it should "be suppressed on the title page and the dedication, which still count as pages" in {
    val doc = paginate(
      manuscript(
        Vector(chapter("One", paragraphOf('a', 60))),
        front = List(FrontMatter.TitlePage, FrontMatter.Dedication("For M."))
      )
    )

    doc.pages.map(_.kind) shouldBe Vector(
      PageKind.Title,
      PageKind.Dedication,
      PageKind.SectionStart,
      PageKind.Body,
      PageKind.Body
    )
    doc.pages.map(_.head.map(_.text)) shouldBe
      Vector(None, None, None, Some("Writer / NIGHT / 4"), Some("Writer / NIGHT / 5"))
  }

  it should "drop an empty segment together with its separator" in {
    val anonymous = manuscript(Vector(chapter("H", paragraphOf('a', 60))))
    val noSurname = anonymous.copy(meta = anonymous.meta.copy(author = anonymous.meta.author.copy(surname = "")))

    paginate(noSurname).pages(1).head.map(_.text) shouldBe Some("NIGHT / 2")
  }

  "page numbers" should "count every page from 1, in order" in {
    val doc = paginate(
      manuscript(
        Vector(chapter("One", paragraphOf('a', 40)), chapter("Two", paragraphOf('b', 3))),
        front = List(FrontMatter.TitlePage)
      )
    )

    doc.pages.map(_.number) shouldBe Vector(1, 2, 3, 4)
  }

  "the title page" should "carry the contact block, word count, centred title and byline" in {
    val doc = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 1))), front = List(FrontMatter.TitlePage)))

    val lines = doc.pages.flatMap(_.lines.take(5))
    doc.pages.map(_.kind) shouldBe Vector(PageKind.Title, PageKind.SectionStart)
    doc.pages.head.lines.map(_.text) shouldBe Vector(
      "Jane Q. Writerabout 86,000 words",
      "1 High Street",
      "Springfield",
      "The Long Night",
      "by Jane Q. Writer"
    )
    lines.head.runs.map(_.x) shouldBe Vector(72f, Margin + 468f - 18 * 6f)
    lines(3).runs.map(_.x) shouldBe Vector(306f - 42f)
    lines(3).baselineY - lines(2).baselineY shouldBe chapterDrop + pitch
  }

  "a dedication" should "sit centred on a page of its own, dropped like a chapter" in {
    val doc = paginate(
      manuscript(Vector(chapter("H", paragraphOf('a', 1))), front = List(FrontMatter.Dedication("For M.")))
    )

    doc.pages.map(_.lines.map(_.text)) shouldBe Vector(Vector("For M."), Vector("H", "a" * 70))
    doc.pages.flatMap(_.lines.headOption).map(_.baselineY).headOption shouldBe Some(Margin + chapterDrop + pitch - 2f)
  }

  "an end marker" should "follow the last line, centred, after a blank line" in {
    val doc   = paginate(manuscript(Vector(chapter("H", paragraphOf('a', 1))), endMarker = Some("END")))
    val lines = doc.pages.flatMap(_.lines)

    lines.map(_.text.take(1)) shouldBe Vector("H", "a", "E")
    lines(2).runs.map(_.x) shouldBe Vector(306f - 9f)
    lines(2).baselineY - lines(1).baselineY shouldBe 2 * pitch
  }

  "inline formatting" should "split a line into runs, each in the face its marks ask for" in {
    val italic = RichTextStyle.empty.withMark(InlineMark.Italic)
    val both   = italic.withMark(InlineMark.Bold)
    val block = Block.Paragraph(
      List(RichTextRun("plain "), RichTextRun("slanted ", italic), RichTextRun("heavy", both)),
      ParagraphKind.Body
    )
    val line = paginate(manuscript(Vector(chapter("H", block)))).pages.flatMap(_.lines)(1)

    line.runs.map(_.text) shouldBe Vector("plain ", "slanted ", "heavy")
    line.runs.map(_.font.style) shouldBe Vector(FaceStyle.Regular, FaceStyle.Italic, FaceStyle.BoldItalic)
    line.runs.map(_.x) shouldBe Vector(108f, 108f + 36f, 108f + 36f + 48f)
    line.runs.map(_.style) shouldBe Vector(RichTextStyle.empty, italic, both)
    line.runs.map(_.font.family).distinct shouldBe Vector(FontFamily.CourierPrime)
  }

  it should "keep a styled run together across a line break, split at the break" in {
    val italic = RichTextStyle.empty.withMark(InlineMark.Italic)
    val words  = Vector.fill(20)("abcde").mkString(" ")
    val block  = Block.Paragraph(List(RichTextRun(words, italic)), ParagraphKind.Body)

    val lines = bodyLines(paginate(manuscript(Vector(chapter("H", block)))))

    lines.flatMap(_.runs).map(_.style).distinct shouldBe Vector(italic)
    lines.map(_.text).mkString shouldBe words
  }

  "centred and quoted paragraphs" should "be centred, and inset by the indent with no first-line indent" in {
    val centred = Block.Paragraph(List(RichTextRun("middle")), ParagraphKind.Centered)
    val quote   = Block.Paragraph(List(RichTextRun("quoted")), ParagraphKind.BlockQuote)
    val lines   = paginate(manuscript(Vector(chapter("H", centred, quote)))).pages.flatMap(_.lines)

    lines(1).runs.map(_.x) shouldBe Vector(306f - 18f)
    lines(2).runs.map(_.x) shouldBe Vector(108f)
  }

  "preformatted text" should "keep every line, blank ones included, without a first-line indent" in {
    val code  = Block.Preformatted(Vector("val a = 1", "", "val b = 2"))
    val lines = bodyLines(paginate(manuscript(Vector(chapter("H", code)))))

    lines.map(_.text) shouldBe Vector("val a = 1", "", "val b = 2")
    lines.flatMap(_.runs.headOption).map(_.x) shouldBe Vector(72f, 72f)
  }

  "an empty manuscript" should "have no pages" in {
    paginate(manuscript(Vector.empty)).pages shouldBe Vector.empty
  }

  "a glyph the font lacks" should "fail the pagination with the measurer's error" in {
    val missing = FixedAdvanceMeasurer(missing = Set(0x4e2d))
    val result  = Paginator.paginate(manuscript(Vector(chapter("H", prose("a中")))), typography, missing)

    result.left.value shouldBe PaginationError.Measure(
      MeasureError.MissingGlyph(0x4e2d, FontFamily.CourierPrime, FaceStyle.Regular)
    )
  }

  "justified typography" should "be refused until justification is implemented" in {
    val result = Paginator.paginate(
      manuscript(Vector(chapter("H", prose("text")))),
      typography.copy(alignment = LineAlignment.Justified),
      measurer
    )

    result.left.value shouldBe PaginationError.UnsupportedAlignment(LineAlignment.Justified)
  }
