package com.serenity

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.Renderer
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
    styledContent.exists(_.element == SyntaxElement.Keyword) shouldBe true
    styledContent.exists(_.element == SyntaxElement.String) shouldBe true
  }

  it should "highlight markdown headings and list markers with markdown-aware styles" in {
    val theme = Theme.dark

    val heading = ThemeManager.highlightLine("# Heading", theme, Some(LanguageId.Markdown))
    heading.head.content shouldBe "# "
    heading.head.style.isBold shouldBe true
    heading(1).content shouldBe "Heading"
    heading(1).style.isBold shouldBe true

    val listItem = ThemeManager.highlightLine("- item", theme, Some(LanguageId.Markdown))
    listItem.head.content shouldBe "- "
    listItem.head.style.isBold shouldBe true
    listItem(1).content shouldBe "item"
  }

  it should "highlight markdown links and blockquotes with markdown-aware styles" in {
    val theme = Theme.dark

    val link = ThemeManager.highlightLine("See [guide](docs.md)", theme, Some(LanguageId.Markdown))
    link.map(_.content) shouldBe List("See ", "[", "guide", "](", "docs.md", ")")
    link(2).style.isUnderlined shouldBe true
    link(4).style.isUnderlined shouldBe true

    val quote = ThemeManager.highlightLine("> quoted", theme, Some(LanguageId.Markdown))
    quote.head.content shouldBe "> "
    quote.head.style.isItalic shouldBe true
    quote(1).content shouldBe "quoted"
    quote(1).style.isItalic shouldBe true
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

  "Renderer" should "render styled text without exceptions" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(0)
    val buffer   = Buffer.fromString(bufferId, "bold text")
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = com.serenity.ui.layout.Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      )
    )
    val surface = new MockRenderSurface(80, 24)
    noException should be thrownBy
      Renderer.render(state, cursorVisible = true, surface, ViewportSize(80, 24))
  }
