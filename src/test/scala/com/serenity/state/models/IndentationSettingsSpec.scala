package com.serenity.state.models

import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IndentationSettingsSpec extends AnyFlatSpec with Matchers:

  "IndentationSettings" should "default to four spaces" in {
    IndentationSettings.Default shouldBe IndentationSettings(tabSize = 4, insertSpaces = true)
    IndentationSettings.Default.indentUnit shouldBe "    "
  }

  it should "indent with one tab character when spaces are off, whatever the tab size" in {
    IndentationSettings(tabSize = 2, insertSpaces = false).indentUnit shouldBe "\t"
    IndentationSettings(tabSize = 8, insertSpaces = false).indentUnit shouldBe "\t"
  }

  it should "indent with tab-size spaces when spaces are on" in {
    IndentationSettings(tabSize = 2).indentUnit shouldBe "  "
    IndentationSettings(tabSize = 8).indentUnit shouldBe "        "
  }

  it should "clamp the tab size into its range" in {
    IndentationSettings(tabSize = 0).normalized.tabSize shouldBe IndentationSettings.MinTabSize
    IndentationSettings(tabSize = -3).normalized.tabSize shouldBe IndentationSettings.MinTabSize
    IndentationSettings(tabSize = 99).normalized.tabSize shouldBe IndentationSettings.MaxTabSize
    IndentationSettings(tabSize = 6).normalized.tabSize shouldBe 6
  }

  it should "never indent by an empty unit, even for an unnormalized tab size" in {
    IndentationSettings(tabSize = 0).indentUnit should not be empty
  }

  "AppConfig" should "expose the indentation settings and normalize them when set" in {
    AppConfig.default.indentation shouldBe IndentationSettings.Default
    AppConfig.default.withIndentation(IndentationSettings(tabSize = 99, insertSpaces = false)).indentation shouldBe
      IndentationSettings(tabSize = IndentationSettings.MaxTabSize, insertSpaces = false)
  }
