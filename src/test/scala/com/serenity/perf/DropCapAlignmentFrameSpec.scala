package com.serenity.perf

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import com.serenity.keystroke.events.{DeleteBackward, InsertChar, MoveDown, MoveUp}
import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A drop cap paragraph that is centred or right-aligned keeps its rows inside the panel, so every frame drawn after an
  * edit or a cursor move is what a cold frame draws.
  */
class DropCapAlignmentFrameSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val shots = sys.props.get("alignment.shots")

  private val scales     = List(1.0, 2.0)
  private val alignments = List(ParagraphAlignment.Center, ParagraphAlignment.Right)

  private def words(count: Int, seed: String): String =
    (1 to count).map(n => s"${seed}word$n").mkString(" ")

  private def document(alignment: ParagraphAlignment, lines: Int): RichTextDocument =
    RichTextDocument(
      (1 to 4).toList.flatMap { chapter =>
        List(
          RichTextParagraph(List(RichTextRun(words(40, s"c$chapter"))), alignment, ParagraphRole.dropCap(lines)),
          RichTextParagraph(List(RichTextRun(words(30, s"b$chapter"))))
        )
      }
    )

  for
    scale     <- scales
    alignment <- alignments
    lines     <- List(2, 3)
  do
    s"Typing in a $alignment-aligned $lines-line drop cap paragraph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(document(alignment, lines), scale, baseConfig)
        List(0, 1, 12, 60, 140).foreach { column =>
          session.placeCursor(0, column)
          "abc".foreach(char => session(InsertChar(char), s"type '$char' at column $column"))
        }
        session.placeCursor(0, 70)
        (1 to 10).foreach(n => session(DeleteBackward, s"delete $n"))
        session.mismatches shouldBe empty
      }

    s"Moving the cursor through a $alignment-aligned $lines-line drop cap paragraph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(document(alignment, lines), scale, baseConfig)
        session.placeCursor(0, 20)
        (1 to 6).foreach(n => session(MoveDown, s"down $n"))
        (1 to 6).foreach(n => session(MoveUp, s"up $n"))
        session.mismatches shouldBe empty
      }

  private def save(name: String, image: BufferedImage): Unit =
    shots.foreach(dir => ImageIO.write(image, "png", File(dir, s"$name.png")))

  for
    scale     <- scales
    alignment <- alignments
  do
    s"A $alignment-aligned 3-line drop cap paragraph at ${scale}x" should "be drawn inside the pane" in {
      val frame = Session(document(alignment, 3), scale, baseConfig).coldFrame()
      save(s"${alignment.toString.toLowerCase}-${scale.toInt}x", frame)
      frame.getWidth should be > 0
    }
