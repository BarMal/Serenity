package com.serenity.richtext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2628: reformatting headings removes the direct formatting a heading's text carries as a whole, so the heading
  * definition shows again, and keeps whatever only part of the heading has.
  */
class HeadingReformatSpec extends AnyFlatSpec with Matchers:

  private val bold      = RichTextStyle(marks = Set(InlineMark.Bold))
  private val italic    = RichTextStyle(marks = Set(InlineMark.Italic))
  private val underline = RichTextStyle(marks = Set(InlineMark.Underline))

  private def heading(level: Int, runs: RichTextRun*): RichTextParagraph =
    RichTextParagraph(runs.toList, role = ParagraphRole.Heading(level))

  private def body(text: String, style: RichTextStyle = RichTextStyle.empty): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(text, style)), role = ParagraphRole.Body)

  private def runsOf(document: RichTextDocument, index: Int): List[RichTextRun] =
    document.paragraphAt(index).getOrElse(fail(s"no paragraph $index")).runs

  private def wholeLine(start: Int, end: Int): RichTextRange =
    RichTextRange(RichTextPosition(start, 0), RichTextPosition(end, 0))

  "Reformatting headings" should "remove every kind of direct formatting a heading carries as a whole" in {
    val everything = RichTextStyle(
      marks = Set(InlineMark.Bold, InlineMark.Italic, InlineMark.Underline),
      fontFamily = Some("Serif"),
      fontSize = Some(20f),
      color = Some("#cc3333")
    )
    val document = RichTextDocument(List(heading(1, RichTextRun("Title", everything))))

    val result = document.reformatHeadings()

    runsOf(result.document, 0) shouldBe List(RichTextRun("Title"))
    result.report shouldBe HeadingReformat.Report(headings = 1, attributes = 6)
  }

  it should "treat a value shared by every run as the heading's own, and merge the runs it leaves alike" in {
    val styled   = RichTextStyle(marks = Set(InlineMark.Bold), fontSize = Some(20f))
    val document = RichTextDocument(List(heading(2, RichTextRun("Chapter ", styled), RichTextRun("One", styled))))

    val result = document.reformatHeadings()

    runsOf(result.document, 0) shouldBe List(RichTextRun("Chapter One"))
    result.report shouldBe HeadingReformat.Report(headings = 1, attributes = 2)
  }

  it should "keep emphasis that only part of the heading has" in {
    val document = RichTextDocument(
      List(heading(1, RichTextRun("Hello ", bold), RichTextRun("World", bold.withMark(InlineMark.Italic))))
    )

    val result = document.reformatHeadings()

    runsOf(result.document, 0) shouldBe List(RichTextRun("Hello "), RichTextRun("World", italic))
    result.report shouldBe HeadingReformat.Report(headings = 1, attributes = 1)
  }

  it should "keep a size, family or colour that differs between runs, or is on only some of them" in {
    val differing = heading(
      1,
      RichTextRun("A", RichTextStyle(fontSize = Some(20f))),
      RichTextRun("B", RichTextStyle(fontSize = Some(24f)))
    )
    val partial = heading(
      2,
      RichTextRun("A", RichTextStyle(fontFamily = Some("Serif"))),
      RichTextRun("B")
    )
    val coloured = heading(
      3,
      RichTextRun("A", RichTextStyle(color = Some("#cc3333"))),
      RichTextRun("B", RichTextStyle(color = Some("#3333cc")))
    )
    val document = RichTextDocument(List(differing, partial, coloured))

    val result = document.reformatHeadings()

    result.document should be theSameInstanceAs document
    result.report shouldBe HeadingReformat.Report.none
  }

  it should "leave a heading's links, raw properties and alignment alone" in {
    val raw = RawProperty("w:kern", "<w:kern w:val=\"2\"/>")
    val styled = RichTextStyle(
      marks = Set(InlineMark.Bold),
      link = Some("https://example.org"),
      extras = List(raw)
    )
    val document = RichTextDocument(
      List(
        RichTextParagraph(
          List(RichTextRun("Linked", styled)),
          alignment = ParagraphAlignment.Center,
          role = ParagraphRole.Heading(1)
        )
      )
    )

    val result    = document.reformatHeadings()
    val paragraph = result.document.paragraphAt(0).getOrElse(fail("no paragraph"))

    paragraph.runs shouldBe List(
      RichTextRun("Linked", RichTextStyle(link = Some("https://example.org"), extras = List(raw)))
    )
    paragraph.alignment shouldBe ParagraphAlignment.Center
    result.report shouldBe HeadingReformat.Report(headings = 1, attributes = 1)
  }

  it should "ignore inline atoms when it decides what the whole heading has, and leave them where they are" in {
    val document = RichTextDocument(
      List(heading(1, RichTextRun("First", bold), RichTextRun.softBreak(), RichTextRun("Second", bold)))
    )

    val result = document.reformatHeadings()

    runsOf(result.document, 0) shouldBe List(
      RichTextRun("First"),
      RichTextRun.softBreak(),
      RichTextRun("Second")
    )
    result.report shouldBe HeadingReformat.Report(headings = 1, attributes = 1)
  }

  it should "leave body paragraphs alone, whatever formatting they carry" in {
    val document = RichTextDocument(List(body("Body text", bold), heading(1, RichTextRun("Title", bold))))

    val result = document.reformatHeadings()

    runsOf(result.document, 0) shouldBe List(RichTextRun("Body text", bold))
    runsOf(result.document, 1) shouldBe List(RichTextRun("Title"))
  }

  it should "do nothing, and say so, when no heading carries direct formatting" in {
    val document = RichTextDocument(List(heading(1, RichTextRun("Title")), body("Body text", bold)))

    val result = document.reformatHeadings()

    result.document should be theSameInstanceAs document
    result.report shouldBe HeadingReformat.Report.none
    result.report.isEmpty shouldBe true
  }

  it should "be idempotent: a second pass finds nothing left to remove" in {
    val document = RichTextDocument(
      List(
        heading(1, RichTextRun("One", bold)),
        body("Between"),
        heading(2, RichTextRun("Two ", underline), RichTextRun("words", underline.withMark(InlineMark.Italic)))
      )
    )

    val once  = document.reformatHeadings()
    val twice = once.document.reformatHeadings()

    once.report shouldBe HeadingReformat.Report(headings = 2, attributes = 2)
    twice.document should be theSameInstanceAs once.document
    twice.report shouldBe HeadingReformat.Report.none
  }

  it should "reformat every heading in the document when it is given no selection" in {
    val document = RichTextDocument(
      List(
        heading(1, RichTextRun("One", bold)),
        body("Between", bold),
        heading(2, RichTextRun("Two", italic)),
        heading(3, RichTextRun("Three", underline))
      )
    )

    val result = document.reformatHeadings()

    List(0, 2, 3).foreach(index => runsOf(result.document, index).map(_.style) shouldBe List(RichTextStyle.empty))
    runsOf(result.document, 1) shouldBe List(RichTextRun("Between", bold))
    result.report shouldBe HeadingReformat.Report(headings = 3, attributes = 3)
  }

  it should "reformat only the headings the selection touches" in {
    val document = RichTextDocument(
      List(
        heading(1, RichTextRun("One", bold)),
        heading(2, RichTextRun("Two", bold)),
        body("Between"),
        heading(2, RichTextRun("Three", bold)),
        heading(1, RichTextRun("Four", bold))
      )
    )

    val result = document.reformatHeadings(Some(wholeLine(1, 3)))

    runsOf(result.document, 0) shouldBe List(RichTextRun("One", bold))
    runsOf(result.document, 1) shouldBe List(RichTextRun("Two"))
    runsOf(result.document, 3) shouldBe List(RichTextRun("Three"))
    runsOf(result.document, 4) shouldBe List(RichTextRun("Four", bold))
    result.report shouldBe HeadingReformat.Report(headings = 2, attributes = 2)
  }

  it should "accept a selection given backwards, and one that runs past the end of the document" in {
    val document = RichTextDocument(List(heading(1, RichTextRun("One", bold)), heading(1, RichTextRun("Two", bold))))

    val backwards = document.reformatHeadings(Some(wholeLine(1, 0)))
    val pastEnd   = document.reformatHeadings(Some(wholeLine(1, 9)))

    backwards.report shouldBe HeadingReformat.Report(headings = 2, attributes = 2)
    runsOf(pastEnd.document, 0) shouldBe List(RichTextRun("One", bold))
    runsOf(pastEnd.document, 1) shouldBe List(RichTextRun("Two"))
  }

  it should "report an empty document as having nothing to reformat" in {
    val result = RichTextDocument(Nil).reformatHeadings()

    result.report shouldBe HeadingReformat.Report.none
  }
