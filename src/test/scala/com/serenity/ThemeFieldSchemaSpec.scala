package com.serenity

import com.serenity.ui.theme.{DefaultThemes, SyntaxElement}
import com.serenity.ui.theme.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThemeFieldSchemaSpec extends AnyFlatSpec with Matchers:

  private val syntaxWithNoOptionalFields = SyntaxColors(
    keyword = SyntaxElementConfig("#5DADE2", Some("#0B0F14"), StyleConfig(bold = true)),
    string = SyntaxElementConfig("#58D68D", Some("#0B0F14")),
    comment = SyntaxElementConfig("#808B96", Some("#0B0F14"), StyleConfig(italic = true)),
    number = SyntaxElementConfig("#F5B041", Some("#0B0F14")),
    operator = SyntaxElementConfig("#EC7063", Some("#0B0F14")),
    identifier = SyntaxElementConfig("#F5F7FA", Some("#0B0F14"))
    // typ/delimiter/whitespace/error/normal deliberately left at their None default
  )

  private val configWithMissingOptionalSyntax =
    ThemeConfig.defaultDark.copy(syntax = syntaxWithNoOptionalFields)

  "ThemeFieldSchema.syntaxFields" should "cover exactly the optional SyntaxColors elements" in {
    ThemeFieldSchema.syntaxFields.map(_.element) should contain theSameElementsAs List(
      SyntaxElement.Type,
      SyntaxElement.Delimiter,
      SyntaxElement.Whitespace,
      SyntaxElement.Error,
      SyntaxElement.Normal
    )
  }

  it should "select None from a config that omits every optional syntax field" in {
    ThemeFieldSchema.syntaxFields.foreach { field =>
      field.select(configWithMissingOptionalSyntax.syntax) shouldBe None
    }
  }

  it should "round-trip: replacing with the schema default and reselecting returns that default" in {
    ThemeFieldSchema.syntaxFields.foreach { field =>
      val updated = field.replace(configWithMissingOptionalSyntax.syntax, field.default)
      field.select(updated) shouldBe Some(field.default)
    }
  }

  "ConfigurableThemeManager" should "fall back to the schema's canonical default for a missing optional syntax field" in {
    val theme = ConfigurableThemeManager.configToTheme(configWithMissingOptionalSyntax).toOption.get

    ThemeFieldSchema.syntaxFields.foreach { field =>
      val expectedForeground = ColorParser.parseColor(field.default.foreground).toOption.get
      val actual              = theme.colorFor(field.element)

      actual.foreground shouldBe expectedForeground
      actual.style.isBold shouldBe field.default.style.bold
      actual.style.isItalic shouldBe field.default.style.italic
      actual.style.isUnderlined shouldBe field.default.style.underline
    }
  }

  "ThemeConfigWriter.render" should "render the schema's canonical default for a missing optional syntax field" in {
    val rendered = ThemeConfigWriter.render(configWithMissingOptionalSyntax)

    ThemeFieldSchema.syntaxFields.foreach { field =>
      rendered should include(s"""foreground = "${field.default.foreground}"""")
    }
  }

  "ThemeCreatorState" should "expose editable rows whose fallback values match the schema default" in {
    // ThemeCreatorState is only ever built from a live Theme via fromTheme, so its draftConfig always has
    // Some(...) for these fields already -- this exercises the descriptor's fallback path directly instead.
    val descriptorPaths = ThemeCreatorState.fromTheme(DefaultThemes.defaultDark).rows.map(_.path)

    List("syntax.type.foreground", "syntax.delimiter.foreground", "syntax.error.foreground", "syntax.normal.foreground")
      .foreach(path => descriptorPaths should contain(path))
  }
