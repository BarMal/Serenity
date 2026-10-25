package com.serenity.perf

import com.serenity.keystroke.events.{InsertChar, MoveDown, MoveUp, PageDown, PageUp, ScrollDown, ScrollUp}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.{FrameBackend, Java2DFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A drop cap's glyph is one opaque box over several rows, painted by its first row only. Every frame that changes a
  * row under the box must therefore repaint all of them, or the glyph is wiped, or left standing over text that moved.
  * Frames are compared with cold ones as [[ColdFrameHarness]] describes.
  */
class DropCapIncrementalFrameSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  // Pinned to Java2D: on Skia an incrementally drawn rich-text frame is the cold frame shifted one device pixel to the
  // left (every pixel matches once shifted), so this suite cannot yet hold Skia to its own cold frame (#2380).
  override protected def referenceFrames: FrameBackend = Java2DFrames

  private def words(count: Int, seed: String): String =
    (1 to count).map(n => s"${seed}word$n").mkString(" ")

  private def dropCapParagraph(lines: Int, wordCount: Int, seed: String): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(words(wordCount, seed))), role = ParagraphRole.dropCap(lines))

  private def bodyParagraph(wordCount: Int, seed: String): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(words(wordCount, seed))))

  /** Drop cap paragraphs of `wordCount` words, each followed by two body paragraphs. */
  private def novel(lines: Int, wordCount: Int, chapters: Int = 30): RichTextDocument =
    RichTextDocument(
      (1 to chapters).toList.flatMap { chapter =>
        List(
          dropCapParagraph(lines, wordCount, s"c$chapter"),
          bodyParagraph(70, s"b$chapter"),
          bodyParagraph(30, s"d$chapter")
        )
      }
    )

  private val scales = List(1.0, 2.0)

  for
    lines <- List(2, 3, 4, 6)
    scale <- scales
  do
    s"Scrolling past a $lines-line drop cap shorter than its glyph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(lines, wordCount = 9), scale)
        (1 to 14).foreach(n => session(ScrollDown(1), s"down $n"))
        (1 to 16).foreach(n => session(ScrollUp(1), s"up $n"))
        (1 to 6).foreach(n => session(ScrollDown(3), s"down by three $n"))
        (1 to 6).foreach(n => session(ScrollUp(3), s"up by three $n"))
        (1 to 4).foreach(n => session(PageDown, s"page down $n"))
        (1 to 4).foreach(n => session(PageUp, s"page up $n"))
        session.mismatches shouldBe empty
      }

  for
    lines <- List(2, 3, 4, 6)
    scale <- scales
  do
    s"Scrolling past a $lines-line drop cap that wraps beyond its glyph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(lines, wordCount = 70), scale)
        (1 to 25).foreach(n => session(ScrollDown(1), s"down $n"))
        (1 to 25).foreach(n => session(ScrollUp(1), s"up $n"))
        (1 to 4).foreach(n => session(PageDown, s"page down $n"))
        (1 to 4).foreach(n => session(PageUp, s"page up $n"))
        session.mismatches shouldBe empty
      }

  for
    lines <- List(2, 3, 4, 6)
    scale <- scales
  do
    s"Typing on the later rows of a $lines-line drop cap paragraph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(lines, wordCount = 90), scale)
        List(100, 220, 330).foreach { column =>
          session.placeCursor(0, column)
          "ab".foreach(char => session(InsertChar(char), s"type '$char' at column $column"))
        }
        session.mismatches shouldBe empty
      }

  for
    lines <- List(2, 3, 4, 6)
    scale <- scales
  do
    s"Moving the cursor between the rows of a $lines-line drop cap paragraph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(lines, wordCount = 90), scale)
        session.placeCursor(0, 20)
        (1 to 8).foreach(n => session(MoveDown, s"down $n"))
        (1 to 8).foreach(n => session(MoveUp, s"up $n"))
        session.mismatches shouldBe empty
      }

  private def withoutDropCaps(document: RichTextDocument): RichTextDocument =
    RichTextDocument(document.paragraphs.map(_.copy(role = ParagraphRole.Body)))

  private def rowsDrawnTyping(document: RichTextDocument, line: Int, column: Int): Long =
    val session = Session(document, scale = 1.0)
    session.markModified()
    session.placeCursor(line, column)
    session(InsertChar('x'), s"type at line $line, column $column")
    session.mismatches shouldBe empty
    session.rowsDrawnByLastFrame

  for lines <- List(2, 3, 6) do
    s"Typing in a $lines-line drop cap paragraph" should
      "draw no more than the glyph's rows beyond what the same paragraph without the cap draws" in {
        val capped   = novel(lines, wordCount = 90)
        val uncapped = withoutDropCaps(capped)
        List(20, 100, 220, 330, 500).foreach { column =>
          val extra = rowsDrawnTyping(capped, 0, column) - rowsDrawnTyping(uncapped, 0, column)
          withClue(s"typing at column $column: ")(extra should be <= lines.toLong)
        }
      }

  "Typing in a body paragraph away from every drop cap" should "draw as many rows as with no drop caps at all" in {
    val capped = novel(3, wordCount = 90)

    rowsDrawnTyping(capped, 1, 300) shouldBe rowsDrawnTyping(withoutDropCaps(capped), 1, 300)
  }
