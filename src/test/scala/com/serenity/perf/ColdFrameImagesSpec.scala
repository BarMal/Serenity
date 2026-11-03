package com.serenity.perf

import java.awt.image.BufferedImage

import com.serenity.FramePixels
import com.serenity.keystroke.events.InsertChar
import com.serenity.richtext.{RichTextDocument, RichTextParagraph, RichTextRun}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The frame a cold repaint draws is the oracle of every incremental frame spec, and drawing it used to allocate two
  * full-window images per comparison: 8 MB each at 1x and 33 MB each at 2x on a 1080p window, every one a humongous
  * allocation. The oracle now draws into images it lends out and takes back. Its caches are still created afresh for
  * every frame, and an image is cleared to what a new one holds before it is lent again, so the frame it draws is the
  * frame a fresh image would give.
  */
class ColdFrameImagesSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private def pixels(image: BufferedImage): Vector[Int] =
    (for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
    yield image.getRGB(x, y)).toVector

  private def pixels(frame: FramePixels): Vector[Int] = frame.rgbPixels.toVector

  private def paragraph(seed: String): RichTextParagraph =
    RichTextParagraph(List(RichTextRun((1 to 60).map(n => s"${seed}word$n").mkString(" "))))

  private val document = RichTextDocument(List(paragraph("alpha"), paragraph("beta"), paragraph("gamma")))

  for scale <- List(1.0, 2.0) do
    s"A cold frame drawn into a reused image at ${scale}x" should
      "equal the frame a fresh image gives, whatever the image held before" in {
        val allocated   = oracleImages.created
        val session     = Session(document, scale)
        val before      = session.currentState
        val freshBefore = pixels(session.coldFrame())
        session(InsertChar('Q'), "edit")
        val after      = session.currentState
        val freshAfter = pixels(session.coldFrame())

        freshBefore should not equal freshAfter

        val firstDirtied = coldFrameWith(after, scale)(pixels)
        val reusedBefore = coldFrameWith(before, scale)(pixels)
        val reusedAfter  = coldFrameWith(after, scale)(pixels)

        firstDirtied shouldBe freshAfter
        reusedBefore shouldBe freshBefore
        reusedAfter shouldBe freshAfter
        oracleImages.created - allocated shouldBe 1
      }

  "Images lent by the oracle" should "be cleared to what a new image holds before they are lent again" in {
    val images = OracleImages()
    val blank  = pixels(BufferedImage(40, 30, BufferedImage.TYPE_INT_RGB))

    images.lend(40, 30, 2) { lent =>
      lent.drawn.foreach(_.stamp(7, 9, 0x123456))
      lent.screen.stamp(11, 3, 0xabcdef)
    }

    images.lend(40, 30, 2) { lent =>
      lent.drawn.map(frame => pixels(frame)).foreach(_ shouldBe blank)
      pixels(lent.screen) shouldBe blank
    }
    images.created shouldBe 1
  }

  it should "lend a second image while the first is in use" in {
    val images = OracleImages()

    images.lend(40, 30, 1)(first => images.lend(40, 30, 1)(second => second should not be theSameInstanceAs(first)))

    images.created shouldBe 2
  }

  it should "keep images of different sizes apart" in {
    val images = OracleImages()

    images.lend(40, 30, 1)(_ => ())
    images.lend(41, 30, 1)(_ => ())
    images.lend(40, 30, 2)(_ => ())

    images.created shouldBe 3
  }

  it should "take an image back when the frame drawn into it fails" in {
    val images = OracleImages()

    an[IllegalStateException] should be thrownBy images.lend(40, 30, 1)(_ => throw IllegalStateException("boom"))
    images.lend(40, 30, 1)(_ => ())

    images.created shouldBe 1
  }
