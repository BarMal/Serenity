package com.serenity.perf

import java.awt.image.BufferedImage

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{
  DeleteBackward,
  InsertChar,
  MoveDown,
  MoveUp,
  PageDown,
  PageUp,
  ScrollDown,
  ScrollUp
}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.state.models.AppState
import com.serenity.ui.openings.{HandDropCapRule, LetterMode, OpeningOverride, OpeningRules, OpeningSettings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A virtual drop cap is painted by the path a hand-made one is, so every frame that changes a row under its glyph, or
  * changes which paragraph has one, must still draw what a cold frame draws: typing in an opening, before it or after
  * it, and turning the setting on and off.
  */
class VirtualDropCapIncrementalFrameSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val on: OpeningSettings = OpeningSettings(enabled = true)

  private val openingsOn: AppConfig  = baseConfig.withOpeningsConfig(on)
  private val openingsOff: AppConfig = baseConfig

  private val scales = List(1.0, 2.0)

  private def words(count: Int, seed: String): String =
    (1 to count).map(n => s"${seed}word$n").mkString(" ")

  private def paragraph(wordCount: Int, seed: String, role: ParagraphRole = ParagraphRole.Body): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(words(wordCount, seed))), role = role)

  private def title(chapter: Int): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(s"Chapter $chapter")), role = ParagraphRole.Heading(1))

  /** Chapters of a title, an opening of `openingWords` words and two body paragraphs; the opening is line `4n + 1`. */
  private def novel(openingWords: Int, chapters: Int = 30): RichTextDocument =
    RichTextDocument(
      (1 to chapters).toList.flatMap { chapter =>
        List(
          title(chapter),
          paragraph(openingWords, s"o$chapter"),
          paragraph(70, s"b$chapter"),
          paragraph(30, s"d$chapter")
        )
      }
    )

  for
    openingWords <- List(9, 70)
    scale        <- scales
  do
    s"Scrolling past $openingWords-word openings with virtual drop caps at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(openingWords), scale, openingsOn)
        (1 to 20).foreach(n => session(ScrollDown(1), s"down $n"))
        (1 to 20).foreach(n => session(ScrollUp(1), s"up $n"))
        (1 to 5).foreach(n => session(ScrollDown(3), s"down by three $n"))
        (1 to 5).foreach(n => session(ScrollUp(3), s"up by three $n"))
        (1 to 4).foreach(n => session(PageDown, s"page down $n"))
        (1 to 4).foreach(n => session(PageUp, s"page up $n"))
        session.mismatches shouldBe empty
      }

  for scale <- scales do
    s"Typing in an opening with a virtual drop cap at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(novel(90), scale, openingsOn)
      List(0, 20, 100, 220, 330).foreach { column =>
        session.placeCursor(1, column)
        "ab".foreach(char => session(InsertChar(char), s"type '$char' at column $column"))
      }
      session.mismatches shouldBe empty
    }

  for scale <- scales do
    s"Moving the cursor between the rows of an opening with a virtual drop cap at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(90), scale, openingsOn)
        session.placeCursor(1, 20)
        (1 to 8).foreach(n => session(MoveDown, s"down $n"))
        (1 to 8).foreach(n => session(MoveUp, s"up $n"))
        session.mismatches shouldBe empty
      }

  for scale <- scales do
    s"Typing before an opening at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(novel(90), scale, openingsOn)
      session.placeCursor(0, 3)
      "title".foreach(char => session(InsertChar(char), s"type '$char' in the title"))
      session.placeCursor(4, 3)
      "next".foreach(char => session(InsertChar(char), s"type '$char' in the next title"))
      session.mismatches shouldBe empty
    }

  /** Each chapter's title, an empty paragraph where an epigraph or blank line might be typed, and then the opening. */
  private def spaced: RichTextDocument =
    RichTextDocument(
      (1 to 12).toList.flatMap { chapter =>
        List(title(chapter), RichTextParagraph.plain(""), paragraph(60, s"o$chapter"), paragraph(40, s"b$chapter"))
      }
    )

  for scale <- scales do
    s"Typing between a title and its opening at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(spaced, scale, openingsOn)
      session.placeCursor(1, 0)
      "the quick".foreach(char => session(InsertChar(char), s"type '$char' above the opening"))
      session.placeCursor(5, 0)
      "ab".foreach(char => session(InsertChar(char), s"type '$char' above the second opening"))
      session.mismatches shouldBe empty
    }

  /** The openings are two characters long, so deleting them hands the status to the paragraph after. */
  private def brief: RichTextDocument =
    RichTextDocument(
      (1 to 12).toList.flatMap { chapter =>
        List(title(chapter), RichTextParagraph.plain("Hi"), paragraph(60, s"b$chapter"), paragraph(40, s"d$chapter"))
      }
    )

  for scale <- scales do
    s"Typing in the paragraph after an opening at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(novel(60), scale, openingsOn)
      session.placeCursor(2, 100)
      "ab".foreach(char => session(InsertChar(char), s"type '$char' after the opening"))
      session.placeCursor(3, 40)
      "cd".foreach(char => session(InsertChar(char), s"type '$char' two after the opening"))
      session.mismatches shouldBe empty
    }

  for scale <- scales do
    s"Emptying an opening so the next paragraph opens the chapter at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(brief, scale, openingsOn)
        session.placeCursor(1, 2)
        (1 to 2).foreach(n => session(DeleteBackward, s"delete $n"))
        "ab".foreach(char => session(InsertChar(char), s"retype '$char'"))
        session.mismatches shouldBe empty
      }

  for scale <- scales do
    s"Changing the role of the paragraph above an opening at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(spaced, scale, openingsOn)
        session.placeCursor(0, 3)
        session.command("paragraph-body", "chapter title becomes body text")
        session.command("heading-1", "chapter title is a title again")
        session.placeCursor(1, 0)
        session.command("heading-1", "blank paragraph above the opening becomes a title")
        session.command("paragraph-body", "and body text again")
        session.mismatches shouldBe empty
      }

  private def withHandMadeOpenings: RichTextDocument =
    RichTextDocument(
      (1 to 12).toList.flatMap { chapter =>
        List(
          title(chapter),
          paragraph(40, s"o$chapter", if chapter % 2 == 0 then ParagraphRole.dropCap(4) else ParagraphRole.Body),
          paragraph(70, s"b$chapter")
        )
      }
    )

  private def inOpenings(update: OpeningSettings => OpeningSettings)(state: AppState): AppState =
    state.copy(persisted =
      state.persisted.copy(config =
        state.persisted.config.withOpeningsConfig(update(state.persisted.config.openingsConfig))
      )
    )

  private def overriding(overrides: OpeningOverride)(state: AppState): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.view
          .mapValues(buffer => buffer.copy(document = buffer.document.copy(openings = overrides)))
          .toMap
      )
    )

  private def dropCaps(enabled: Boolean)(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(config = state.persisted.config.withDropCapsEnabled(enabled)))

  for scale <- scales do
    s"Changing the openings settings at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(withHandMadeOpenings, scale, openingsOff)
      session.change("openings on")(inOpenings(_.copy(enabled = true)))
      session.change("hand drop cap yields")(
        inOpenings(settings => settings.copy(rules = OpeningRules(handDropCap = HandDropCapRule.OpeningWins)))
      )
      session.change("hand drop cap with small caps")(
        inOpenings(settings => settings.copy(rules = OpeningRules(handDropCap = HandDropCapRule.HandPlusSmallCaps)))
      )
      session.change("raised letter")(inOpenings(_.copy(letterMode = LetterMode.Raised)))
      session.change("drop letter")(inOpenings(_.copy(letterMode = LetterMode.Drop)))
      session.change("drop caps disabled")(dropCaps(enabled = false))
      session.change("drop caps enabled")(dropCaps(enabled = true))
      session.change("document turns openings off")(overriding(OpeningOverride(enabled = Some(false))))
      session.change("document turns openings on")(overriding(OpeningOverride(enabled = Some(true))))
      session.change("openings off")(inOpenings(_.copy(enabled = false)))
      session.change("document override cleared")(overriding(OpeningOverride.Empty))
      session.mismatches shouldBe empty
    }

  for scale <- scales do
    s"Scrolling a document whose openings are being toggled at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(9), scale, openingsOn)
        (1 to 6).foreach(n => session(ScrollDown(2), s"down $n"))
        session.change("openings off")(inOpenings(_.copy(enabled = false)))
        (1 to 6).foreach(n => session(ScrollUp(2), s"up $n"))
        session.change("openings on")(inOpenings(_.copy(enabled = true)))
        session.mismatches shouldBe empty
      }

  private def rowsDrawnTyping(config: AppConfig, document: RichTextDocument, line: Int, column: Int): Long =
    val session = Session(document, scale = 1.0, config)
    session.markModified()
    session.placeCursor(line, column)
    session(InsertChar('x'), s"type at line $line, column $column")
    session.mismatches shouldBe empty
    session.rowsDrawnByLastFrame

  "Typing in an opening with a virtual drop cap" should
    "draw no more than the glyph's rows beyond what the same paragraph without openings draws" in {
      val document = novel(90)
      List(20, 100, 220, 330, 500).foreach { column =>
        val extra =
          rowsDrawnTyping(openingsOn, document, 1, column) - rowsDrawnTyping(openingsOff, document, 1, column)
        withClue(s"typing at column $column: ")(extra should be <= 2L)
      }
    }

  "Typing in a body paragraph away from every opening" should "draw as many rows as with openings off" in {
    val document = novel(90)

    rowsDrawnTyping(openingsOn, document, 2, 300) shouldBe rowsDrawnTyping(openingsOff, document, 2, 300)
  }

  private def coldFrame(config: AppConfig, document: RichTextDocument): BufferedImage =
    Session(document, scale = 1.0, config).coldFrame()

  "A cold frame with openings off" should "be pixel-identical to one that never heard of openings" in {
    val document = novel(60, chapters = 6)
    val before   = coldFrame(baseConfig, document)

    List(
      baseConfig.withOpeningsConfig(on.copy(enabled = false, letterMode = LetterMode.Raised)),
      baseConfig.withOpeningsConfig(on.copy(enabled = false, letterMode = LetterMode.Hybrid, smallCapsFirstLine = true))
    ).foreach { config =>
      val frame = coldFrame(config, document)
      val differs = for
        x <- 0 until before.getWidth
        y <- 0 until before.getHeight
        if before.getRGB(x, y) != frame.getRGB(x, y)
      yield (x, y)
      differs shouldBe empty
    }
  }

  "A cold frame with a virtual drop cap" should "differ from one without" in {
    val document = novel(60, chapters = 6)
    val before   = coldFrame(openingsOff, document)
    val after    = coldFrame(openingsOn, document)

    (for
      x <- 0 until before.getWidth
      y <- 0 until before.getHeight
      if before.getRGB(x, y) != after.getRGB(x, y)
    yield (x, y)) should not be empty
  }

  "A cold frame with a raised or hybrid letter" should "differ from one without openings and from a drop cap's" in {
    val document = novel(60, chapters = 6)
    val frames = List(openingsOff, openingsOn).map(coldFrame(_, document)) ++
      List(LetterMode.Raised, LetterMode.Hybrid).map(mode =>
        coldFrame(baseConfig.withOpeningsConfig(on.copy(letterMode = mode)), document)
      )
    def differs(a: BufferedImage, b: BufferedImage): Boolean =
      (for
        x <- 0 until a.getWidth
        y <- 0 until a.getHeight
        if a.getRGB(x, y) != b.getRGB(x, y)
      yield (x, y)).nonEmpty

    frames.combinations(2).foreach(pair => differs(pair.head, pair(1)) shouldBe true)
  }
