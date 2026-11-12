package com.serenity

import com.serenity.markdown.Emphasis
import com.serenity.richtext.InlineMark
import com.serenity.state.models.MarkdownSelection
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Markdown has no syntax for a shadow, so it neither reads nor writes one. */
class MarkdownShadowSpec extends AnyFlatSpec with Matchers:

  "MarkdownSelection.emphasisOf" should "have no emphasis for Shadow and the same ones as before for the rest" in {
    MarkdownSelection.emphasisOf(InlineMark.Shadow) shouldBe None
    MarkdownSelection.emphasisOf(InlineMark.Bold) shouldBe Some(Emphasis.Bold)
    MarkdownSelection.emphasisOf(InlineMark.Italic) shouldBe Some(Emphasis.Italic)
    MarkdownSelection.emphasisOf(InlineMark.Underline) shouldBe Some(Emphasis.Underline)
  }
end MarkdownShadowSpec
