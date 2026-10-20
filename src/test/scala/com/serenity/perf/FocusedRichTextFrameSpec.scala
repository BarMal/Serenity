package com.serenity.perf

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import com.serenity.keystroke.events.{MoveDown, MoveUp}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Focus mode in a rich-text document, whose paragraphs are one line each with no blank line between them (#1853), dims
  * every paragraph but the one holding the cursor.
  */
class FocusedRichTextFrameSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val shots = sys.props.get("focus.shots")

  private def paragraph(n: Int): RichTextParagraph =
    RichTextParagraph(
      List(RichTextRun(s"Paragraph $n opens here. " + ("It keeps going with some ordinary prose. " * 3))),
      role = ParagraphRole.Body
    )

  private val document = RichTextDocument((1 to 7).toList.map(paragraph))

  private def frame(focused: Boolean, scale: Double): BufferedImage =
    val session = Session(document, scale, baseConfig.withFocusedTextBody(focused))
    session.placeCursor(3, 4)
    session.coldFrame()

  private def save(name: String, image: BufferedImage): Unit =
    shots.foreach(dir => ImageIO.write(image, "png", File(dir, s"$name.png")))

  /** The brightest pixel of each row of the text area, above the status line. */
  private def rowBrightness(image: BufferedImage, scale: Double): IndexedSeq[Int] =
    (0 until (500 * scale).toInt).map { y =>
      (0 until image.getWidth).map { x =>
        val rgb = image.getRGB(x, y)
        ((rgb >> 16 & 0xff) + (rgb >> 8 & 0xff) + (rgb & 0xff)) / 3
      }.max
    }

  private val inkFloor = 60

  for scale <- List(1.0, 2.0) do
    s"A rich-text document with focus mode on at ${scale}x" should
      "dim every paragraph but the one holding the cursor" in {
        val on = frame(focused = true, scale)
        save(s"focus-off-${scale.toInt}x", frame(focused = false, scale))
        save(s"focus-on-${scale.toInt}x", on)
        val brightness = rowBrightness(on, scale)
        val ink        = brightness.indices.filter(brightness(_) > inkFloor)
        val brightest  = brightness.max
        val active     = ink.filter(brightness(_) >= brightest * 95 / 100)
        val dimmed     = ink.filterNot(active.contains)
        withClue("the cursor's paragraph stays bright: ")(active should not be empty)
        withClue("paragraphs above it are dimmed: ")(dimmed.exists(_ < active.min) shouldBe true)
        withClue("paragraphs below it are dimmed: ")(dimmed.exists(_ > active.max) shouldBe true)
        withClue("the bright rows are one paragraph, two wrapped lines: ") {
          (active.max - active.min) should be < (60 * scale).toInt
        }
      }

    s"Moving the cursor between rich-text paragraphs with focus mode on at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(document, scale, baseConfig.withFocusedTextBody(true))
        session.placeCursor(0, 4)
        (1 to 6).foreach(n => session(MoveDown, s"down $n"))
        (1 to 6).foreach(n => session(MoveUp, s"up $n"))
        session.mismatches shouldBe empty
      }
