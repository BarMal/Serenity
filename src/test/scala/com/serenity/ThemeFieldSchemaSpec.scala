package com.serenity

import com.serenity.ui.theme.config.*
import com.serenity.ui.theme.{DefaultThemes, SyntaxElement}
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

  it should "select None from a config that omits every optional syntax field" in
    ThemeFieldSchema.syntaxFields.foreach(field => field.select(configWithMissingOptionalSyntax.syntax) shouldBe None)

  it should "round-trip: replacing with the schema default and reselecting returns that default" in
    ThemeFieldSchema.syntaxFields.foreach { field =>
      val updated = field.replace(configWithMissingOptionalSyntax.syntax, field.default)
      field.select(updated) shouldBe Some(field.default)
    }

  "ConfigurableThemeManager" should "fall back to the schema's canonical default for a missing optional syntax field" in {
    val theme = ConfigurableThemeManager.configToTheme(configWithMissingOptionalSyntax).toOption.get

    ThemeFieldSchema.syntaxFields.foreach { field =>
      val expectedForeground = ColorParser.parseColor(field.default.foreground).toOption.get
      val actual             = theme.colorFor(field.element)

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

  // ── Mandatory field coverage (issue #1410) ──────────────────────────────────
  //
  // ConfigurableThemeManager (config -> domain), ThemeConfigWriter (domain -> config -> text) and ThemeCreatorState
  // (creator UI rows) each independently hand-enumerated the MANDATORY theme fields too, with no shared source of
  // truth -- so a field added to one could silently be missing from another. These tests pin every mandatory-field
  // consumer's actual behavior to `ThemeFieldSchema`'s field lists, so a field present in the schema but not wired
  // into one of the three consumers fails here.

  "ThemeFieldSchema.uiScalarFields" should "cover exactly the mandatory scalar UiColors fields" in {
    ThemeFieldSchema.uiScalarFields.map(_.path) should contain theSameElementsAs List(
      "ui.foreground",
      "ui.background",
      "ui.cursor",
      "ui.border",
      "ui.muted",
      "ui.placeholder"
    )
  }

  "ThemeFieldSchema.uiTokenFields" should "cover exactly the mandatory UiTokenConfig UiColors fields" in {
    ThemeFieldSchema.uiTokenFields.map(_.path) should contain theSameElementsAs List(
      "ui.highlighted",
      "ui.menu-item",
      "ui.panel",
      "ui.error"
    )
  }

  "ThemeFieldSchema.mandatorySyntaxFields" should "cover exactly the mandatory SyntaxColors elements" in {
    ThemeFieldSchema.mandatorySyntaxFields.map(_.element) should contain theSameElementsAs List(
      SyntaxElement.Keyword,
      SyntaxElement.String,
      SyntaxElement.Comment,
      SyntaxElement.Number,
      SyntaxElement.Operator,
      SyntaxElement.Identifier
    )
  }

  "ThemeCreatorState" should "expose an editable row for every mandatory scalar UI field in the schema" in {
    val paths = ThemeCreatorState.fromTheme(DefaultThemes.defaultDark).rows.map(_.path)
    ThemeFieldSchema.uiScalarFields.foreach(field => paths should contain(field.path))
  }

  it should "expose foreground/background rows for every mandatory UI token field in the schema" in {
    val paths = ThemeCreatorState.fromTheme(DefaultThemes.defaultDark).rows.map(_.path)
    ThemeFieldSchema.uiTokenFields.foreach { field =>
      paths should contain(s"${field.path}.foreground")
      paths should contain(s"${field.path}.background")
    }
  }

  it should "expose a row for every mandatory syntax field in the schema" in {
    val paths = ThemeCreatorState.fromTheme(DefaultThemes.defaultDark).rows.map(_.path)
    ThemeFieldSchema.mandatorySyntaxFields.foreach(field => paths should contain(field.path))
  }

  "ThemeConfigWriter.render" should "render every mandatory field declared in the schema" in {
    val rendered = ThemeConfigWriter.render(ThemeConfig.defaultDark)

    ThemeFieldSchema.uiScalarFields.foreach { field =>
      rendered should include(s"""${field.hoconKey} = "${field.select(ThemeConfig.defaultDark.ui)}"""")
    }
    ThemeFieldSchema.mandatorySyntaxFields.foreach { field =>
      rendered should include(s"""foreground = "${field.select(ThemeConfig.defaultDark.syntax).foreground}"""")
    }
  }

  "ConfigurableThemeManager.configToTheme" should
    "produce a Theme whose mandatory scalar UI colors match the config, for every schema field" in {
      val theme = ConfigurableThemeManager.configToTheme(ThemeConfig.defaultDark).toOption.get

      ThemeFieldSchema.uiScalarFields.foreach { field =>
        val expected = ColorParser.parseColor(field.select(ThemeConfig.defaultDark.ui)).toOption.get
        field.themeValue(theme) shouldBe expected
      }
    }

  it should "produce a Theme whose mandatory UI token colors match the config, for every schema field" in {
    val theme = ConfigurableThemeManager.configToTheme(ThemeConfig.defaultDark).toOption.get

    ThemeFieldSchema.uiTokenFields.foreach { field =>
      val expectedForeground = ColorParser.parseColor(field.select(ThemeConfig.defaultDark.ui).foreground).toOption.get
      field.themeValue(theme).foreground shouldBe expectedForeground
    }
  }

  it should "produce a Theme whose mandatory syntax colors match the config, for every schema field" in {
    val theme = ConfigurableThemeManager.configToTheme(ThemeConfig.defaultDark).toOption.get

    ThemeFieldSchema.mandatorySyntaxFields.foreach { field =>
      val expected =
        ColorParser.parseColor(field.select(ThemeConfig.defaultDark.syntax).foreground).toOption.get
      theme.colorFor(field.element).foreground shouldBe expected
    }
  }
