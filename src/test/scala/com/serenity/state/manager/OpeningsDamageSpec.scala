package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.openings.{LetterMode, OpeningOverride, OpeningSettings, RaisedRowRule}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A paragraph can gain or lose a virtual drop cap because of an edit elsewhere, and a screen that shows the frame
  * before last cannot find out from the frames alone, so the edit's damage has to name the paragraph.
  */
class OpeningsDamageSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val base     = AppState.initial
  private val bufferId = BufferId(0)

  private def title(text: String)                      = RichTextParagraph.plain(text, role = ParagraphRole.Heading(1))
  private def body(text: String)                       = RichTextParagraph.plain(text)
  private def document(paragraphs: RichTextParagraph*) = RichTextDocument(paragraphs.toList)

  private val on = AppConfig.default.withOpeningsConfig(OpeningSettings(enabled = true))

  private def stateOf(
    doc: RichTextDocument,
    config: AppConfig,
    overrides: OpeningOverride = OpeningOverride.Empty
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, doc.plainText)
      .copy(richText = RichTextState(richTextDocument = Some(doc)))
    val configured = buffer.copy(document = buffer.document.copy(openings = overrides))
    base.copy(persisted =
      base.persisted.copy(buffers = base.persisted.buffers.updated(bufferId, configured), config = config)
    )

  private def damagedLines(before: AppState, after: AppState): Set[Int] =
    val damage = DamageProducer.forTransition(before, after)
    Damage.damagedLines(bufferId, damage)

  private val chapter = document(title("One"), body(""), body("It was a bright cold day."), body("Second paragraph."))

  "Typing in a blank paragraph above an opening" should "damage the opening, which it takes the status from" in {
    val typed =
      document(title("One"), body("Epigraph words"), body("It was a bright cold day."), body("Second paragraph."))

    damagedLines(stateOf(chapter, on), stateOf(typed, on)) should contain(2)
  }

  "Emptying an opening" should "damage the paragraph that becomes the opening" in {
    val short   = document(title("One"), body("Hi"), body("Next paragraph."), body("Last."))
    val emptied = document(title("One"), body(""), body("Next paragraph."), body("Last."))

    damagedLines(stateOf(short, on), stateOf(emptied, on)) should contain(2)
  }

  "Editing a title" should "damage the opening below it" in {
    val retitled = document(title("One!"), body(""), body("It was a bright cold day."), body("Second paragraph."))

    damagedLines(stateOf(chapter, on), stateOf(retitled, on)) should contain(2)
  }

  "Typing in an ordinary body paragraph" should "damage nothing beyond the paragraph" in {
    val before = document(title("One"), body("It was a bright cold day."), body("Second paragraph."), body("Third."))
    val typed  = document(title("One"), body("It was a bright cold day."), body("Second paragraph.x"), body("Third."))

    damagedLines(stateOf(before, on), stateOf(typed, on)) shouldBe Set(2)
  }

  "Typing in an opening" should "damage the opening alone" in {
    val before = document(title("One"), body("It was a bright cold day."), body("Second paragraph."), body("Third."))
    val typed  = document(title("One"), body("It was a bright cold day.x"), body("Second paragraph."), body("Third."))

    damagedLines(stateOf(before, on), stateOf(typed, on)) shouldBe Set(1)
  }

  "With openings off, an edit" should "damage exactly the lines it did before" in {
    val typed =
      document(title("One"), body("Epigraph words"), body("It was a bright cold day."), body("Second paragraph."))
    val offDamage = damagedLines(stateOf(chapter, AppConfig.default), stateOf(typed, AppConfig.default))

    offDamage shouldBe Set(1)
    List(LetterMode.Raised, LetterMode.Hybrid).foreach { mode =>
      val config = AppConfig.default.withOpeningsConfig(OpeningSettings(letterMode = mode))
      damagedLines(stateOf(chapter, config), stateOf(typed, config)) shouldBe offDamage
    }
  }

  "With a raised or hybrid letter, an edit above an opening" should "damage the opening, as with a drop cap" in {
    val typed =
      document(title("One"), body("Epigraph words"), body("It was a bright cold day."), body("Second paragraph."))

    List(LetterMode.Raised, LetterMode.Hybrid).foreach { mode =>
      val config =
        AppConfig.default.withOpeningsConfig(
          OpeningSettings(enabled = true, letterMode = mode, raisedRow = RaisedRowRule.Clamp)
        )
      damagedLines(stateOf(chapter, config), stateOf(typed, config)) should contain(2)
    }
  }

  it should "damage the whole buffer instead when the row grows with the letter, since the opening's row changes height" in {
    val typed =
      document(title("One"), body("Epigraph words"), body("It was a bright cold day."), body("Second paragraph."))

    List(LetterMode.Raised, LetterMode.Hybrid).foreach { mode =>
      val config = AppConfig.default.withOpeningsConfig(OpeningSettings(enabled = true, letterMode = mode))
      everyRowDamaged(stateOf(chapter, config), stateOf(typed, config)) shouldBe true
    }
  }

  private def inMode(mode: LetterMode, rule: RaisedRowRule) =
    AppConfig.default.withOpeningsConfig(OpeningSettings(enabled = true, letterMode = mode, raisedRow = rule))

  private def everyRowDamaged(before: AppState, after: AppState): Boolean =
    val damage = DamageProducer.forTransition(before, after)
    Damage.isEverything(damage) || Damage.damagesEveryRow(bufferId, damage)

  private val opening = document(title("One"), body("It was a bright cold day."), body("Second paragraph."))

  "With a letter that makes its row grow, an edit to the first letter of a paragraph" should
    "damage the whole buffer, since the row below it moves with the letter" in {
      val retyped = document(title("One"), body("Xt was a bright cold day."), body("Second paragraph."))
      val typed   = document(title("One"), body("XIt was a bright cold day."), body("Second paragraph."))

      List(LetterMode.Raised, LetterMode.Hybrid).foreach { mode =>
        val config = inMode(mode, RaisedRowRule.Grow)
        everyRowDamaged(stateOf(opening, config), stateOf(retyped, config)) shouldBe true
        everyRowDamaged(stateOf(opening, config), stateOf(typed, config)) shouldBe true
      }
    }

  it should "damage the whole buffer when a line is added, which may hand the opening to another paragraph" in {
    val split  = document(title("One"), body("It was a bright"), body(" cold day."), body("Second paragraph."))
    val config = inMode(LetterMode.Raised, RaisedRowRule.Grow)

    everyRowDamaged(stateOf(opening, config), stateOf(split, config)) shouldBe true
  }

  it should "damage only the paragraph for an edit further into it" in {
    val typed = document(title("One"), body("It was a bright cold dayx."), body("Second paragraph."))

    damagedLines(
      stateOf(opening, inMode(LetterMode.Raised, RaisedRowRule.Grow)),
      stateOf(typed, inMode(LetterMode.Raised, RaisedRowRule.Grow))
    ) shouldBe
      Set(1)
  }

  "With a letter that does not make its row grow, an edit to the first letter" should
    "damage only the paragraph" in {
      val retyped = document(title("One"), body("Xt was a bright cold day."), body("Second paragraph."))

      List(
        inMode(LetterMode.Raised, RaisedRowRule.Clamp),
        inMode(LetterMode.Hybrid, RaisedRowRule.Clamp),
        inMode(LetterMode.Drop, RaisedRowRule.Grow)
      ).foreach(config => damagedLines(stateOf(opening, config), stateOf(retyped, config)) shouldBe Set(1))
    }

  "Changing the raised row rule or the lines a hybrid letter sinks" should "damage the whole buffer" in
    List(
      OpeningSettings(enabled = true, letterMode = LetterMode.Raised) ->
        OpeningSettings(enabled = true, letterMode = LetterMode.Raised, raisedRow = RaisedRowRule.Clamp),
      OpeningSettings(enabled = true, letterMode = LetterMode.Hybrid) ->
        OpeningSettings(enabled = true, letterMode = LetterMode.Hybrid, hybridSinkLines = 4)
    ).foreach { (was, now) =>
      val damage = DamageProducer.forTransition(
        stateOf(chapter, AppConfig.default.withOpeningsConfig(was)),
        stateOf(chapter, AppConfig.default.withOpeningsConfig(now))
      )

      (Damage.isEverything(damage) || Damage.damagesEveryRow(bufferId, damage)) shouldBe true
    }

  "Changing a document's openings override" should "damage the whole buffer" in {
    val damage = DamageProducer.forTransition(
      stateOf(chapter, on),
      stateOf(chapter, on, OpeningOverride(enabled = Some(false)))
    )

    Damage.damagesEveryRow(bufferId, damage) shouldBe true
  }

  "Restating the global setting in a document's override" should "damage nothing" in {
    val damage = DamageProducer.forTransition(
      stateOf(chapter, on),
      stateOf(chapter, on, OpeningOverride(enabled = Some(true)))
    )

    Damage.damagedLines(bufferId, damage) shouldBe empty
  }
