package com.serenity.ui.openings

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.ui.layout.RichTextContext
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** With openings on and `letter = drop`, the resolver declares each opening paragraph a drop cap without the document
  * itself changing.
  */
class VirtualDropCapRoleSpec extends AnyFlatSpec with Matchers:

  private val title   = RichTextParagraph.plain("Chapter One", role = ParagraphRole.Heading(1))
  private val opening = RichTextParagraph.plain("It was a bright cold day in April.")
  private val second  = RichTextParagraph.plain("The clocks were striking thirteen.")

  private val document = RichTextDocument(List(title, opening, second, title, opening, second))

  private val on = OpeningSettings(enabled = true)

  private def roles(
    settings: OpeningSettings,
    document: RichTextDocument = document,
    dropCapsEnabled: Boolean = true
  ): Int => ParagraphRole =
    val context =
      RichTextContext(Some(document), 1.0f, dropCapsEnabled, virtualDropCaps = VirtualDropCaps.of(settings, document))
    line => context.layoutRole(line)

  private def withHand(rule: HandDropCapRule, lines: Int = 5): (RichTextDocument, OpeningSettings) =
    val handMade = RichTextDocument(List(title, opening.copy(role = ParagraphRole.dropCap(lines)), second))
    (handMade, on.copy(rules = OpeningRules(handDropCap = rule)))

  "An opening paragraph" should "lay out as a three-line drop cap while openings are on" in {
    val role = roles(on)

    List(1, 4).foreach(line => role(line) shouldBe ParagraphRole.dropCap(VirtualDropCaps.OpeningLines))
  }

  it should "leave the document's own role untouched" in {
    roles(on)(1)
    document.paragraphAt(1).map(_.role) shouldBe Some(ParagraphRole.Body)
  }

  it should "leave every other paragraph as it is" in {
    val role = roles(on)

    List(0, 2, 3, 5).foreach(line => role(line) shouldBe document.paragraphAt(line).map(_.role).get)
  }

  it should "lay out as body text while openings are off" in
    List(1, 4).foreach(line => roles(OpeningSettings.default)(line) shouldBe ParagraphRole.Body)

  it should "lay out as body text under a raised letter" in
    List(LetterMode.Raised, LetterMode.Hybrid).foreach { mode =>
      List(1, 4).foreach(line => roles(on.copy(letterMode = mode))(line) shouldBe ParagraphRole.Body)
    }

  it should "lay out as body text while drop caps are disabled" in
    List(1, 4).foreach(line => roles(on, dropCapsEnabled = false)(line) shouldBe ParagraphRole.Body)

  "A paragraph after a scene break" should "become a drop cap only where the rules give it a full opening" in {
    val scene                       = RichTextDocument(List(title, opening, RichTextParagraph.plain("***"), second))
    def after(rule: SceneBreakRule) = roles(on.copy(rules = OpeningRules(sceneBreak = rule)), scene)(3)

    after(SceneBreakRule.FullOpening) shouldBe ParagraphRole.dropCap(VirtualDropCaps.OpeningLines)
    after(SceneBreakRule.SmallCapsOnly) shouldBe ParagraphRole.Body
    after(SceneBreakRule.NoOpening) shouldBe ParagraphRole.Body
  }

  "A hand-made drop cap on an opening" should "be kept under hand" in {
    val (handMade, settings) = withHand(HandDropCapRule.HandWins)

    roles(settings, handMade)(1) shouldBe ParagraphRole.dropCap(5)
  }

  it should "be kept under hand-small-caps, whose small caps come later" in {
    val (handMade, settings) = withHand(HandDropCapRule.HandPlusSmallCaps)

    roles(settings, handMade)(1) shouldBe ParagraphRole.dropCap(5)
  }

  it should "give way to the opening's drop cap under opening" in {
    val (handMade, settings) = withHand(HandDropCapRule.OpeningWins)

    roles(settings, handMade)(1) shouldBe ParagraphRole.dropCap(VirtualDropCaps.OpeningLines)
  }

  it should "be left on a paragraph that is not an opening whatever the rule" in {
    val handMade = RichTextDocument(List(title, opening, second.copy(role = ParagraphRole.dropCap(4))))

    roles(on.copy(rules = OpeningRules(handDropCap = HandDropCapRule.OpeningWins)), handMade)(2) shouldBe
      ParagraphRole.dropCap(4)
  }

  "The effective role of an opening" should "differ from the role the paragraph is stored with" in {
    val context =
      RichTextContext(Some(document), 1.0f, dropCapsEnabled = true, virtualDropCaps = VirtualDropCaps.of(on, document))

    context.roles.roleFor(document, 1) should not be EffectiveRole.Real(ParagraphRole.Body)
  }
