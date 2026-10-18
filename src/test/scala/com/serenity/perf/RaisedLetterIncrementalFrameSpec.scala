package com.serenity.perf

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
import com.serenity.ui.color.RenderColor
import com.serenity.ui.openings.{
  HandDropCapRule,
  LetterMode,
  OpeningOverride,
  OpeningRules,
  OpeningSettings,
  RaisedRowRule
}
import com.serenity.ui.theme.DropCapColour
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A raised or hybrid letter is painted by the path a drop cap is, but it can reach above its row: in `clamp` into the
  * row before it, in `grow` into room its own row made. Every frame that changes a row it covers, or the row before it,
  * or what the letter is, must draw what a cold frame draws.
  */
class RaisedLetterIncrementalFrameSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val scales = List(1.0, 2.0)

  private def settings(mode: LetterMode, rule: RaisedRowRule, sink: Int = 2): OpeningSettings =
    OpeningSettings(enabled = true, letterMode = mode, raisedRow = rule, hybridSinkLines = sink)

  private val variants: List[(String, OpeningSettings)] = List(
    "raised, grow"    -> settings(LetterMode.Raised, RaisedRowRule.Grow),
    "raised, clamp"   -> settings(LetterMode.Raised, RaisedRowRule.Clamp),
    "hybrid 2, grow"  -> settings(LetterMode.Hybrid, RaisedRowRule.Grow, 2),
    "hybrid 3, clamp" -> settings(LetterMode.Hybrid, RaisedRowRule.Clamp, 3),
    "hybrid 2, clamp" -> settings(LetterMode.Hybrid, RaisedRowRule.Clamp, 2)
  )

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

  private def spaced: RichTextDocument =
    RichTextDocument(
      (1 to 12).toList.flatMap { chapter =>
        List(title(chapter), RichTextParagraph.plain(""), paragraph(60, s"o$chapter"), paragraph(40, s"b$chapter"))
      }
    )

  private def brief: RichTextDocument =
    RichTextDocument(
      (1 to 12).toList.flatMap { chapter =>
        List(title(chapter), RichTextParagraph.plain("Hi"), paragraph(60, s"b$chapter"), paragraph(40, s"d$chapter"))
      }
    )

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

  private def inConfig(update: AppConfig => AppConfig)(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(config = update(state.persisted.config)))

  private def overriding(overrides: OpeningOverride)(state: AppState): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.view
          .mapValues(buffer => buffer.copy(document = buffer.document.copy(openings = overrides)))
          .toMap
      )
    )

  for
    (variant, letter) <- variants
    scale             <- scales
  do
    val config = baseConfig.withOpeningsConfig(letter)

    for openingWords <- List(9, 70) do
      s"Scrolling past $openingWords-word openings with a $variant letter at ${scale}x" should
        "draw what a cold frame draws at every step" in {
          val session = Session(novel(openingWords), scale, config)
          (1 to 20).foreach(n => session(ScrollDown(1), s"down $n"))
          (1 to 20).foreach(n => session(ScrollUp(1), s"up $n"))
          (1 to 5).foreach(n => session(ScrollDown(3), s"down by three $n"))
          (1 to 5).foreach(n => session(ScrollUp(3), s"up by three $n"))
          (1 to 4).foreach(n => session(PageDown, s"page down $n"))
          (1 to 4).foreach(n => session(PageUp, s"page up $n"))
          session.mismatches shouldBe empty
        }

    s"Typing in an opening with a $variant letter at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(novel(90), scale, config)
      List(0, 0, 20, 100, 220, 330).foreach { column =>
        session.placeCursor(1, column)
        "ab".foreach(char => session(InsertChar(char), s"type '$char' at column $column"))
      }
      session.mismatches shouldBe empty
    }

    s"Changing the letter of an opening with a $variant letter at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(40), scale, config)
        "QjgWiT".foreach { char =>
          session.placeCursor(1, 0)
          session(InsertChar(char), s"type '$char' in front of the opening")
        }
        "QjgWiT".foreach { _ =>
          session.placeCursor(1, 1)
          session(DeleteBackward, "delete the first letter")
        }
        "Eg".foreach { char =>
          session.placeCursor(1, 0)
          session(InsertChar(char), s"type '$char' as the first letter")
        }
        session.mismatches shouldBe empty
      }

    s"Moving the cursor between the rows of an opening with a $variant letter at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(90), scale, config)
        session.placeCursor(1, 20)
        (1 to 8).foreach(n => session(MoveDown, s"down $n"))
        (1 to 8).foreach(n => session(MoveUp, s"up $n"))
        session.mismatches shouldBe empty
      }

    s"Typing before an opening with a $variant letter at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(90), scale, config)
        session.placeCursor(0, 3)
        "title".foreach(char => session(InsertChar(char), s"type '$char' in the title"))
        session.placeCursor(4, 3)
        "next".foreach(char => session(InsertChar(char), s"type '$char' in the next title"))
        session.placeCursor(3, 10)
        "tail".foreach(char => session(InsertChar(char), s"type '$char' in the paragraph before a title"))
        session.mismatches shouldBe empty
      }

    s"Typing between a title and its opening with a $variant letter at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(spaced, scale, config)
        session.placeCursor(1, 0)
        "the quick".foreach(char => session(InsertChar(char), s"type '$char' above the opening"))
        session.placeCursor(5, 0)
        "ab".foreach(char => session(InsertChar(char), s"type '$char' above the second opening"))
        session.mismatches shouldBe empty
      }

    s"Typing after an opening with a $variant letter at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(60), scale, config)
        session.placeCursor(2, 100)
        "ab".foreach(char => session(InsertChar(char), s"type '$char' after the opening"))
        session.placeCursor(3, 40)
        "cd".foreach(char => session(InsertChar(char), s"type '$char' two after the opening"))
        session.mismatches shouldBe empty
      }

    s"Emptying an opening so the next paragraph opens the chapter with a $variant letter at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(brief, scale, config)
        session.placeCursor(1, 2)
        (1 to 2).foreach(n => session(DeleteBackward, s"delete $n"))
        "ab".foreach(char => session(InsertChar(char), s"retype '$char'"))
        session.mismatches shouldBe empty
      }

    s"Changing the role of the paragraph above an opening with a $variant letter at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(spaced, scale, config)
        session.placeCursor(0, 3)
        session.command("paragraph-body", "chapter title becomes body text")
        session.command("heading-1", "chapter title is a title again")
        session.placeCursor(1, 0)
        session.command("heading-1", "blank paragraph above the opening becomes a title")
        session.command("paragraph-body", "and body text again")
        session.mismatches shouldBe empty
      }

    s"Changing the gap and colour beside a $variant letter at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(60), scale, config)
        List(0.0f, 1.0f, 2.0f, 0.25f, 0.5f).foreach { gap =>
          session.change(s"gap $gap")(inConfig(_.withDropCapGapEm(gap)))
          session(ScrollDown(2), s"scroll down at gap $gap")
          session(ScrollUp(2), s"scroll up at gap $gap")
        }
        List(DropCapColour.Text, DropCapColour.Heading, DropCapColour.Fixed(RenderColor.fromRgba(220, 30, 30)))
          .foreach { colour =>
            session.change(s"colour $colour")(inConfig(_.withDropCapColour(colour)))
            session(ScrollDown(3), s"scroll down in $colour")
            session(ScrollUp(3), s"scroll up in $colour")
          }
        session.mismatches shouldBe empty
      }

    s"Typing beside a $variant letter in a changed gap and colour at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(90), scale, config)
        session.change("wide gap")(inConfig(_.withDropCapGapEm(1.5f)))
        session.change("accent colour")(inConfig(_.withDropCapColour(DropCapColour.Accent)))
        session.placeCursor(1, 0)
        "xy".foreach(char => session(InsertChar(char), s"type '$char' before the letter"))
        session.placeCursor(1, 120)
        "zw".foreach(char => session(InsertChar(char), s"type '$char' beside the letter"))
        session.mismatches shouldBe empty
      }

  for scale <- scales do
    s"Changing the openings settings at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(withHandMadeOpenings, scale, baseConfig)
      session.change("openings on")(inOpenings(_.copy(enabled = true)))
      session.change("raised letter")(inOpenings(_.copy(letterMode = LetterMode.Raised)))
      session.change("raised row clamps")(inOpenings(_.copy(raisedRow = RaisedRowRule.Clamp)))
      session.change("raised row grows")(inOpenings(_.copy(raisedRow = RaisedRowRule.Grow)))
      session.change("hybrid letter")(inOpenings(_.copy(letterMode = LetterMode.Hybrid)))
      (2 to 6).foreach(sink => session.change(s"hybrid sinks $sink")(inOpenings(_.copy(hybridSinkLines = sink))))
      session.change("hybrid row clamps")(inOpenings(_.copy(raisedRow = RaisedRowRule.Clamp)))
      (6 to 2 by -2).foreach(sink => session.change(s"hybrid sinks $sink")(inOpenings(_.copy(hybridSinkLines = sink))))
      session.change("hand drop cap yields")(
        inOpenings(settings => settings.copy(rules = OpeningRules(handDropCap = HandDropCapRule.OpeningWins)))
      )
      session.change("hand drop cap with small caps")(
        inOpenings(settings => settings.copy(rules = OpeningRules(handDropCap = HandDropCapRule.HandPlusSmallCaps)))
      )
      session.change("raised letter again")(inOpenings(_.copy(letterMode = LetterMode.Raised)))
      session.change("drop letter")(inOpenings(_.copy(letterMode = LetterMode.Drop)))
      session.change("hybrid letter again")(inOpenings(_.copy(letterMode = LetterMode.Hybrid)))
      session.change("drop caps disabled")(inConfig(_.withDropCapsEnabled(false)))
      session.change("drop caps enabled")(inConfig(_.withDropCapsEnabled(true)))
      session.change("document turns openings off")(overriding(OpeningOverride(enabled = Some(false))))
      session.change("document turns openings on")(overriding(OpeningOverride(enabled = Some(true))))
      session.change("document raises the letter")(
        overriding(OpeningOverride(enabled = Some(true), letterMode = Some(LetterMode.Raised)))
      )
      session.change("document clamps the row")(
        overriding(
          OpeningOverride(
            enabled = Some(true),
            letterMode = Some(LetterMode.Raised),
            raisedRow = Some(RaisedRowRule.Clamp)
          )
        )
      )
      session.change("document sinks four lines")(
        overriding(OpeningOverride(letterMode = Some(LetterMode.Hybrid), hybridSinkLines = Some(4)))
      )
      session.change("openings off")(inOpenings(_.copy(enabled = false)))
      session.change("document override cleared")(overriding(OpeningOverride.Empty))
      session.mismatches shouldBe empty
    }

  for scale <- scales do
    s"Scrolling a document whose letter style is being toggled at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session =
          Session(novel(9), scale, baseConfig.withOpeningsConfig(settings(LetterMode.Raised, RaisedRowRule.Clamp)))
        (1 to 6).foreach(n => session(ScrollDown(2), s"down $n"))
        session.change("hybrid")(inOpenings(_.copy(letterMode = LetterMode.Hybrid)))
        (1 to 6).foreach(n => session(ScrollUp(2), s"up $n"))
        session.change("grow")(inOpenings(_.copy(raisedRow = RaisedRowRule.Grow)))
        (1 to 3).foreach(n => session(ScrollDown(3), s"down by three $n"))
        session.change("drop")(inOpenings(_.copy(letterMode = LetterMode.Drop)))
        session.change("raised")(inOpenings(_.copy(letterMode = LetterMode.Raised)))
        session.mismatches shouldBe empty
      }
