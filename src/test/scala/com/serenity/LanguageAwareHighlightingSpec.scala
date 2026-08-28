package com.serenity

import com.serenity.lsp.config.LanguageId
import com.serenity.ui.theme.{LexState, SyntaxElement, Theme, ThemeManager}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Characterization tests for issue #859: syntax coloring must not apply Scala-specific keyword semantics to languages
  * that do not have them, and lexical state (open comments / open strings) must carry correctly across line boundaries.
  */
class LanguageAwareHighlightingSpec extends AnyFlatSpec with Matchers:

  private val theme = Theme.dark

  "highlightLine" should "not color a JavaScript identifier that happens to be a Scala keyword" in {
    // `trait` is a plain identifier in JavaScript, not a keyword.
    val styled = ThemeManager.highlightLine("const trait = 1;", theme, Some(LanguageId.JavaScript))

    styled.map(_.content).mkString shouldBe "const trait = 1;"
    styled.exists(s => s.content == "trait" && s.style == theme.colorFor(SyntaxElement.Keyword).style) shouldBe false
  }

  it should "not color a TypeScript property named `class`" in {
    val styled = ThemeManager.highlightLine("interface Foo { class: string }", theme, Some(LanguageId.TypeScript))

    styled.exists(s => s.content == "class" && s.style == theme.colorFor(SyntaxElement.Keyword).style) shouldBe false
  }

  it should "not color a Python variable named `case` as a Scala keyword" in {
    val styled = ThemeManager.highlightLine("case = \"some_value\"", theme, Some(LanguageId.Python))

    styled.exists(s => s.content == "case" && s.style == theme.colorFor(SyntaxElement.Keyword).style) shouldBe false
  }

  it should "render unsupported languages as a single safe plain-text run" in {
    val pythonLine = ThemeManager.highlightLine("def foo(): return True", theme, Some(LanguageId.Python))
    pythonLine shouldBe List(
      com.serenity.ui.theme
        .StyledText(pythonLine.head.content, com.serenity.ui.theme.TextStyle.normal, theme.foreground, theme.background)
    )
    pythonLine.head.content shouldBe "def foo(): return True"
  }

  it should "render JSON as plain text rather than applying Scala token rules" in {
    val jsonLine = ThemeManager.highlightLine("""{"class": true, "trait": 1}""", theme, Some(LanguageId.JsonLang))

    jsonLine.size shouldBe 1
    jsonLine.head.content shouldBe """{"class": true, "trait": 1}"""
  }

  it should "still apply keyword-aware highlighting for Scala" in {
    val styled = ThemeManager.highlightLine("val x = 1", theme, Some(LanguageId.Scala))

    styled.exists(s => s.content == "val" && s.style == theme.colorFor(SyntaxElement.Keyword).style) shouldBe true
  }

  "Scala multiline lexical state" should "carry an unterminated block comment across line boundaries" in {
    val lines = Vector(
      "val x = 1 /* start",
      "still inside the comment",
      "end */ val y = 2"
    )

    val startStates = ThemeManager.lineStartStates("multiline-comment-doc", lines, Some(LanguageId.Scala))
    startStates shouldBe Vector(LexState.Default, LexState.InBlockComment, LexState.InBlockComment)

    val secondLine = ThemeManager.highlightLine(lines(1), theme, Some(LanguageId.Scala), startStates(1))
    secondLine.size shouldBe 1
    secondLine.head.content shouldBe "still inside the comment"
    secondLine.head.style shouldBe theme.colorFor(SyntaxElement.Comment).style

    val thirdLine = ThemeManager.highlightLine(lines(2), theme, Some(LanguageId.Scala), startStates(2))
    thirdLine.head.content shouldBe "end */"
    thirdLine.head.style shouldBe theme.colorFor(SyntaxElement.Comment).style
    thirdLine.exists(s => s.content == "val" && s.style == theme.colorFor(SyntaxElement.Keyword).style) shouldBe true
  }

  it should "correctly scope a triple-quoted string that opens and closes on the same line" in {
    val styled = ThemeManager.highlightLine("val s = \"\"\"hello\"\"\" + 1", theme, Some(LanguageId.Scala))

    styled.exists(s =>
      s.content == "\"\"\"hello\"\"\"" && s.style == theme.colorFor(SyntaxElement.String).style
    ) shouldBe true
  }

  it should "carry an unterminated triple-quoted string across line boundaries" in {
    val lines = Vector(
      "val s = \"\"\"first line",
      "second line still in the string\"\"\"",
      "val y = 2"
    )

    val startStates = ThemeManager.lineStartStates("multiline-string-doc", lines, Some(LanguageId.Scala))
    startStates shouldBe Vector(LexState.Default, LexState.InTripleQuotedString, LexState.Default)

    val secondLine = ThemeManager.highlightLine(lines(1), theme, Some(LanguageId.Scala), startStates(1))
    secondLine.head.content shouldBe "second line still in the string\"\"\""
    secondLine.head.style shouldBe theme.colorFor(SyntaxElement.String).style
  }

  it should "incrementally recompute lexical state only from the edited line onward" in {
    val original = Vector(
      "val a = 1",
      "val b = /* open",
      "still open",
      "closed */ val c = 3"
    )
    val initialStates = ThemeManager.lineStartStates("incremental-doc", original, Some(LanguageId.Scala))
    initialStates shouldBe Vector(LexState.Default, LexState.Default, LexState.InBlockComment, LexState.InBlockComment)

    // Editing only the first line must not change the (already-correct) state of downstream lines, and the
    // recomputed index must still reflect that edit correctly.
    val edited       = original.updated(0, "val a = 100")
    val editedStates = ThemeManager.lineStartStates("incremental-doc", edited, Some(LanguageId.Scala))
    editedStates shouldBe Vector(LexState.Default, LexState.Default, LexState.InBlockComment, LexState.InBlockComment)
  }

  "LanguageId.fromString" should "recognize the languages covered by these contract tests" in {
    LanguageId.fromString("python") shouldBe Some(LanguageId.Python)
    LanguageId.fromString("javascript") shouldBe Some(LanguageId.JavaScript)
    LanguageId.fromString("typescript") shouldBe Some(LanguageId.TypeScript)
    LanguageId.fromString("json") shouldBe Some(LanguageId.JsonLang)
    LanguageId.fromString("scala") shouldBe Some(LanguageId.Scala)
  }
