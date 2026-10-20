package com.serenity.perf

import java.awt.image.BufferedImage

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{InsertChar, ScrollDown, ScrollUp}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.state.models.AppState
import com.serenity.ui.color.RenderColor
import com.serenity.ui.openings.OpeningSettings
import com.serenity.ui.theme.DropCapColour
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The gap beside a drop cap moves every row that wraps beside it, and its colour repaints the glyph's rows: a frame
  * drawn after changing either must be the frame a cold start draws. Hand-made and virtual drop caps share one path, so
  * both are covered.
  */
class DropCapLookIncrementalFrameSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val scales = List(1.0, 2.0)

  private def words(count: Int, seed: String): String =
    (1 to count).map(n => s"${seed}word$n").mkString(" ")

  private def paragraph(wordCount: Int, seed: String, role: ParagraphRole = ParagraphRole.Body): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(words(wordCount, seed))), role = role)

  private def title(chapter: Int): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(s"Chapter $chapter")), role = ParagraphRole.Heading(1))

  private def handMade: RichTextDocument =
    RichTextDocument(
      (1 to 20).toList.flatMap { chapter =>
        List(paragraph(60, s"c$chapter", ParagraphRole.dropCap(3)), paragraph(70, s"b$chapter"))
      }
    )

  private def novel: RichTextDocument =
    RichTextDocument(
      (1 to 20).toList.flatMap { chapter =>
        List(title(chapter), paragraph(60, s"o$chapter"), paragraph(70, s"b$chapter"))
      }
    )

  private val virtual: AppConfig = baseConfig.withOpeningsConfig(OpeningSettings(enabled = true))

  private val documents: List[(String, RichTextDocument, AppConfig)] =
    List(("hand-made", handMade, baseConfig), ("virtual", novel, virtual))

  private def inConfig(update: AppConfig => AppConfig)(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(config = update(state.persisted.config)))

  private val custom = DropCapColour.Fixed(RenderColor.fromRgba(220, 30, 30))

  for
    (kind, document, config) <- documents
    scale                    <- scales
  do
    s"Changing the gap beside a $kind drop cap at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(document, scale, config)
      List(0.0f, 1.0f, 2.0f, 0.25f, 0.5f).foreach { gap =>
        session.change(s"gap $gap")(inConfig(_.withDropCapGapEm(gap)))
        session(ScrollDown(2), s"scroll down at gap $gap")
        session(ScrollUp(2), s"scroll up at gap $gap")
      }
      session.mismatches shouldBe empty
    }

    s"Changing the colour of a $kind drop cap at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(document, scale, config)
      List(DropCapColour.Text, DropCapColour.Heading, custom, DropCapColour.Accent).foreach { colour =>
        session.change(s"colour $colour")(inConfig(_.withDropCapColour(colour)))
        session(ScrollDown(3), s"scroll down in $colour")
        session(ScrollUp(3), s"scroll up in $colour")
      }
      session.mismatches shouldBe empty
    }

    s"Typing beside a $kind drop cap in a changed gap and colour at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(document, scale, config)
        session.change("wide gap")(inConfig(_.withDropCapGapEm(1.5f)))
        session.change("custom colour")(inConfig(_.withDropCapColour(custom)))
        List(10, 80, 200).foreach { column =>
          session.placeCursor(if kind == "virtual" then 1 else 0, column)
          "ab".foreach(char => session(InsertChar(char), s"type '$char' at column $column"))
        }
        session.mismatches shouldBe empty
      }

  private def redPixels(frame: BufferedImage): Int =
    (for
      x <- 0 until frame.getWidth
      y <- 0 until frame.getHeight
      rgb = frame.getRGB(x, y)
      if ((rgb >> 16) & 0xff) > 180 && ((rgb >> 8) & 0xff) < 80 && (rgb & 0xff) < 80
    yield 1).size

  for (kind, document, config) <- documents do
    s"A cold frame of a $kind drop cap" should "paint the glyph in the configured colour and nothing else in it" in {
      val red   = Session(document, 1.0, config.withDropCapColour(custom)).coldFrame()
      val plain = Session(document, 1.0, config.withDropCapColour(DropCapColour.Text)).coldFrame()

      redPixels(red) should be > 0
      redPixels(plain) shouldBe 0
    }
