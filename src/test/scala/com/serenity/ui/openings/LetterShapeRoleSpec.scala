package com.serenity.ui.openings

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A virtual initial letter carries, besides the role it lays out with, the shape it sits in: a drop cap's, or one that
  * rises above its first line. Paragraphs whose drop cap the document made keep the drop cap's shape.
  */
class LetterShapeRoleSpec extends AnyFlatSpec with Matchers:

  private val title   = RichTextParagraph.plain("Chapter One", role = ParagraphRole.Heading(1))
  private val opening = RichTextParagraph.plain("It was a bright cold day in April.")
  private val second  = RichTextParagraph.plain("The clocks were striking thirteen.")

  private val document = RichTextDocument(List(title, opening, second, title, opening, second))

  private val on = OpeningSettings(enabled = true)

  private def withHand(rule: HandDropCapRule, lines: Int = 5): (RichTextDocument, OpeningSettings) =
    val handMade = RichTextDocument(List(title, opening.copy(role = ParagraphRole.dropCap(lines)), second))
    (handMade, on.copy(rules = OpeningRules(handDropCap = rule)))

  "A virtual initial letter" should "keep a drop cap's shape under a drop letter and rise under a raised or hybrid one" in {
    def shape(settings: OpeningSettings): LetterShape =
      val virtual = VirtualDropCaps.of(settings, document)
      ParagraphRoles(OpeningContext(true, virtual), ChapterOpenings.standard).shapeAt(document, 1)

    shape(on) shouldBe LetterShape.Drop
    shape(on.copy(letterMode = LetterMode.Raised)) shouldBe LetterShape.Rising(growsRow = true)
    shape(on.copy(letterMode = LetterMode.Raised, raisedRow = RaisedRowRule.Clamp)) shouldBe
      LetterShape.Rising(growsRow = false)
    shape(on.copy(letterMode = LetterMode.Hybrid, raisedRow = RaisedRowRule.Clamp)) shouldBe
      LetterShape.Rising(growsRow = false)
  }

  it should "have no shape of its own where the document keeps a drop cap" in {
    val (handMade, settings) = withHand(HandDropCapRule.HandWins)
    val virtual              = VirtualDropCaps.of(settings.copy(letterMode = LetterMode.Raised), handMade)

    ParagraphRoles(OpeningContext(true, virtual), ChapterOpenings.standard).shapeAt(handMade, 1) shouldBe
      LetterShape.Drop
  }

  it should "take the shape of the opening when the opening's drop cap wins over a hand-made one" in {
    val (handMade, settings) = withHand(HandDropCapRule.OpeningWins)
    val virtual              = VirtualDropCaps.of(settings.copy(letterMode = LetterMode.Raised), handMade)
    val roles                = ParagraphRoles(OpeningContext(true, virtual), ChapterOpenings.standard)

    roles.layoutRole(handMade, 1) shouldBe ParagraphRole.dropCap(1)
    roles.shapeAt(handMade, 1) shouldBe LetterShape.Rising(growsRow = true)
  }
