package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.TextColor
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.theme.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThemeSupportSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Theme" should "define basic color scheme" in {
    val darkTheme = Theme.dark

    darkTheme.name shouldBe "dark"
    darkTheme.foregroundColor should not be null
    darkTheme.backgroundColor should not be null
    darkTheme.cursorColor should not be null
  }

  it should "define text styling options" in {
    val theme = Theme.dark

    theme.textStyle.isBold shouldBe false
    theme.textStyle.isItalic shouldBe false
    theme.textStyle.isUnderlined shouldBe false
  }

  it should "provide syntax highlighting colors" in {
    val theme = Theme.dark

    theme.syntaxColors should contain key SyntaxElement.Keyword
    theme.syntaxColors should contain key SyntaxElement.String
    theme.syntaxColors should contain key SyntaxElement.Comment
    theme.syntaxColors should contain key SyntaxElement.Number
  }

  "ThemeManager" should "apply theme to buffer content" in {
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "function hello() { return 'world'; }")
    val theme    = Theme.dark

    val styledContent = ThemeManager.applyTheme(buffer.content, theme)

    styledContent should not be empty
    // Should contain styled segments for different syntax elements
    styledContent.exists(_.element == SyntaxElement.Keyword) shouldBe true
    styledContent.exists(_.element == SyntaxElement.String) shouldBe true
  }

  "StyledText" should "combine content with styling information" in {
    val styledText = StyledText("hello", TextStyle(isBold = true, isItalic = false))

    styledText.content shouldBe "hello"
    styledText.style.isBold shouldBe true
    styledText.style.isItalic shouldBe false
  }

  "TextStyle" should "support bold, italic, underline combinations" in {
    val style1 = TextStyle(isBold = true, isItalic = false, isUnderlined = false)
    val style2 = TextStyle(isBold = false, isItalic = true, isUnderlined = false)
    val style3 = TextStyle(isBold = true, isItalic = true, isUnderlined = true)

    style1.isBold shouldBe true
    style2.isItalic shouldBe true
    style3.isBold shouldBe true
    style3.isItalic shouldBe true
    style3.isUnderlined shouldBe true
  }

  "ThemeRenderer" should "render styled text with proper Lanterna formatting" in {
    import com.googlecode.lanterna.graphics.TextGraphics
    import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
    import com.serenity.ui.layout.Layout
    import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal

    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen   = new TerminalScreen(virtualTerminal)
    val graphics = screen.newTextGraphics()

    val styledText = StyledText(
      "bold text",
      TextStyle(isBold = true, isItalic = false, isUnderlined = false),
      TextColor.ANSI.WHITE
    )

    // Should not throw an exception
    noException should be thrownBy
      ThemeRenderer.renderStyledText(graphics, 0, 0, styledText)
  }
