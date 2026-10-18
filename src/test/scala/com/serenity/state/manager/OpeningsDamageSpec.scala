package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.openings.{LetterMode, OpeningOverride, OpeningSettings}
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

  "With openings off, or a raised letter, an edit" should "damage exactly the lines it did before" in {
    val typed =
      document(title("One"), body("Epigraph words"), body("It was a bright cold day."), body("Second paragraph."))
    val offDamage = damagedLines(stateOf(chapter, AppConfig.default), stateOf(typed, AppConfig.default))

    offDamage shouldBe Set(1)
    List(
      AppConfig.default.withOpeningsConfig(OpeningSettings(enabled = true, letterMode = LetterMode.Raised)),
      AppConfig.default.withOpeningsConfig(OpeningSettings(enabled = true, letterMode = LetterMode.Hybrid))
    ).foreach(config => damagedLines(stateOf(chapter, config), stateOf(typed, config)) shouldBe offDamage)
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
