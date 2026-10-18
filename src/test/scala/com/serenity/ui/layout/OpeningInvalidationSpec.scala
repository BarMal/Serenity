package com.serenity.ui.layout

import com.serenity.richtext.{
  ParagraphAlignment,
  ParagraphRole,
  RichTextDocument,
  RichTextParagraph,
  RichTextPosition,
  RichTextRange
}
import com.serenity.rope.{Balance, ChangeSet, Rope}
import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A paragraph can gain or lose chapter-opening status without being touched, because the heading or scene break above
  * it was. The widened edit must reach it, without reaching paragraphs an edit cannot affect.
  */
class OpeningInvalidationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val chapters = Vector(
    "# One",
    "",
    "First para.",
    "",
    "Second para.",
    "",
    "# Two",
    "",
    "Two para."
  )

  private def rope(lines: Seq[String]): Rope = Rope(lines.mkString("\n"))

  private def measured(content: Rope): VisualLineIndex =
    (0 until content.lineCount).foldLeft(VisualLineIndex.unmeasured(content.lineCount))((index, line) =>
      index.measured(line, 1)
    )

  private def unmeasuredLines(index: VisualLineIndex): Set[Int] =
    (0 until index.lineCount).filter(index.measuredRows(_).isEmpty).toSet

  private def byEdit(before: Seq[String], after: Seq[String]): Set[Int] =
    val (from, to) = (rope(before), rope(after))
    unmeasuredLines(VisualLineIndexStore.followEdit(measured(from), from, to))

  private def byChange(before: Seq[String], after: Seq[String]): Set[Int] =
    val (from, to) = (rope(before), rope(after))
    val change     = ChangeSet.fromDiff(from, to)
    unmeasuredLines(VisualLineIndexStore.followChange(measured(from), from, to, change))

  private def bothWays(before: Seq[String], after: Seq[String])(check: Set[Int] => Unit): Unit =
    withClue("followEdit: ")(check(byEdit(before, after)))
    withClue("followChange: ")(check(byChange(before, after)))

  "Editing a chapter heading" should "also unmeasure the opening paragraph below it" in
    bothWays(chapters, chapters.updated(0, "# Uno"))(_ shouldBe Set(0, 2))

  it should "unmeasure the opening when the heading marker is removed" in
    bothWays(chapters, chapters.updated(0, "One"))(_ shouldBe Set(0, 2))

  it should "unmeasure the opening when a plain line becomes a heading" in
    bothWays(chapters, chapters.updated(4, "# Second para."))(_ shouldBe Set(4))

  it should "unmeasure the paragraph that becomes the opening when a heading line is inserted" in {
    val inserted = chapters.patch(2, Seq("# New"), 0)
    bothWays(chapters, inserted)(unmeasured => unmeasured should contain allOf (2, 3))
  }

  it should "unmeasure the paragraph that becomes the opening when a heading line is deleted" in {
    val removed = chapters.patch(6, Nil, 1)
    bothWays(chapters, removed)(unmeasured => unmeasured should contain(7))
  }

  "Editing the lines between a heading and its opening" should "unmeasure the paragraph that becomes the opening" in {
    val padded = Vector("# T", "", "", "Para.")
    bothWays(padded, padded.updated(1, "Typed"))(unmeasured => unmeasured should contain(3))
  }

  "Editing the opening paragraph" should "unmeasure the paragraph after it, which becomes the opening if it empties" in
    bothWays(chapters, chapters.updated(2, ""))(unmeasured => unmeasured should contain(4))

  "Editing a scene break" should "unmeasure the paragraph after it" in {
    val scene = Vector("Intro.", "", "***", "", "After the break.", "", "More.")
    bothWays(scene, scene.updated(2, "**"))(unmeasured => unmeasured should contain(4))
    bothWays(scene, scene.updated(2, "* * *"))(unmeasured => unmeasured should contain(4))
    bothWays(scene, scene.updated(0, "***"))(_ shouldBe Set(0))
  }

  "Editing a setext underline" should "unmeasure the title above it and the paragraph below it" in {
    val setext = Vector("Title", "=====", "", "Para.", "", "Later.")
    bothWays(setext, setext.updated(1, "text"))(_ shouldBe Set(0, 1, 3))
  }

  "Editing prose far from any heading" should "unmeasure nothing beyond the edited line" in {
    bothWays(chapters, chapters.updated(4, "Second paragraph, longer."))(_ shouldBe Set(4))
    bothWays(chapters, chapters.updated(8, "Two paragraph, longer."))(_ shouldBe Set(8))
  }

  it should "also unmeasure the paragraph after it when its text merely changes, as that one would open if this emptied" in
    bothWays(chapters, chapters.updated(2, "First paragraph, longer."))(_ shouldBe Set(2, 4))

  private def visualLine(bufferLine: Int, text: String): TextVisualLine =
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = 0,
      endColumn = text.length,
      text = text,
      widthPx = text.length * 8.0f,
      caretStops = (0 to text.length).toVector.map(offset => TextCaretStop(offset, offset * 8.0f))
    )

  private def richSnapshot(document: RichTextDocument): TextLayoutSnapshot =
    TextLayoutSnapshot(
      visualLines =
        document.paragraphs.zipWithIndex.map((paragraph, line) => visualLine(line, paragraph.plainText)).toVector,
      panelWidthPx = 320,
      lineHeightPx = 16,
      ascentPx = 13,
      richTextDocument = Some(document)
    )

  private def paragraphs(specs: (String, ParagraphRole, ParagraphAlignment)*): RichTextDocument =
    RichTextDocument(
      specs.toList.map((text, role, alignment) => RichTextParagraph.plain(text, alignment, role))
    ).normalized

  private def body(text: String)    = (text, ParagraphRole.Body, ParagraphAlignment.Left)
  private def title(text: String)   = (text, ParagraphRole.Heading(1), ParagraphAlignment.Left)
  private def centred(text: String) = (text, ParagraphRole.Body, ParagraphAlignment.Center)

  private def retyped(document: RichTextDocument, paragraph: Int, text: String): RichTextDocument =
    val current = document.paragraphAt(paragraph).map(_.plainTextLength).getOrElse(0)
    document.replaceRange(RichTextRange(RichTextPosition(paragraph, 0), RichTextPosition(paragraph, current)), text)

  private def dirty(before: RichTextDocument, after: RichTextDocument): Set[Int] =
    DirtyLineDiff.dirtyRows(Some(richSnapshot(before)), richSnapshot(after))

  private val manuscript =
    paragraphs(title("Chapter"), body(""), body("Opening."), body(""), body("Second."), body("Third."))

  "DirtyLineDiff with chapter headings" should "repaint the opening when its heading's text changes" in
    (dirty(manuscript, retyped(manuscript, 0, "Chapter One")) shouldBe Set(0, 2))

  it should "repaint the paragraph after a scene break when the break changes" in {
    val scene = paragraphs(body("Intro."), body("***"), body("After."), body("More."))
    dirty(scene, retyped(scene, 1, "**")) shouldBe Set(1, 2)
  }

  it should "repaint the opening when an epigraph between it and the heading changes" in {
    val epigraph = paragraphs(title("Chapter"), centred("To the sea"), body("Opening."), body("More."))
    dirty(epigraph, retyped(epigraph, 1, "To the sky")) shouldBe Set(1, 2)
  }

  it should "repaint the next paragraph when the opening empties" in
    (dirty(manuscript, retyped(manuscript, 2, "")) shouldBe Set(2, 4))

  it should "repaint nothing beyond an edited paragraph that no heading governs" in {
    dirty(manuscript, retyped(manuscript, 4, "Second, longer.")) shouldBe Set(4)
    dirty(manuscript, retyped(manuscript, 2, "Opening, longer.")) shouldBe Set(2)
  }
