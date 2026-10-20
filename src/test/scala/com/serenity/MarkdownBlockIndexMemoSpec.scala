package com.serenity

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.markdown.MarkdownBlockIndexMemo
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownBlockIndexMemoSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def contentOf(text: String) = Buffer.fromString(BufferId(1), text).document.content

  private val memo = MarkdownBlockIndexMemo()

  "MarkdownBlockIndexMemo" should "hand back the same index for the same content, without scanning it again" in {
    val content = contentOf("```\ncode\n```\n\n| a | b |\n|---|---|\n")

    val first = memo.of(content)

    (memo.of(content): AnyRef) should be theSameInstanceAs first
    first.ranges shouldBe Vector(0 to 2)
    first.tables shouldBe Vector(4 to 5)
  }

  it should "index edited content afresh" in {
    val before = memo.of(contentOf("a\n```\nb\n```"))
    val after  = memo.of(contentOf("```\nb\n```"))

    before.ranges shouldBe Vector(1 to 3)
    after.ranges shouldBe Vector(0 to 2)
  }

  it should "keep several buffers' indexes at once" in {
    val one = contentOf("# one")
    val two = contentOf("# two\n```\nx\n```")

    val indexOfOne = memo.of(one)
    val indexOfTwo = memo.of(two)

    (memo.of(one): AnyRef) should be theSameInstanceAs indexOfOne
    (memo.of(two): AnyRef) should be theSameInstanceAs indexOfTwo
  }

  it should "make no difference to the equality of the state holding it" in {
    MarkdownBlockIndexMemo() shouldBe MarkdownBlockIndexMemo()
  }

  "TextStyle.styledFont" should "strike through the font of struck text" in {
    val font = TextStyle.styledFont(Font(Font.SERIF, Font.PLAIN, 16), TextStyle(isStrikethrough = true))

    font.getAttributes.get(TextAttribute.STRIKETHROUGH) shouldBe TextAttribute.STRIKETHROUGH_ON
    TextStyle.normal.combine(TextStyle(isStrikethrough = true)).isStrikethrough shouldBe true
  }
