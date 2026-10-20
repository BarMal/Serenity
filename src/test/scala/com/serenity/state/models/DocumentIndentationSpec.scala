package com.serenity.state.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How a document's effective indentation is settled from the sources that can speak for it (#1996). */
class DocumentIndentationSpec extends AnyFlatSpec with Matchers:

  private val global = IndentationSettings(tabSize = 4, insertSpaces = true)

  private val twoSpaces  = IndentationHint.of(IndentationSettings(tabSize = 2))
  private val eightTabs  = IndentationHint.of(IndentationSettings(tabSize = 8, insertSpaces = false))
  private val tabsOnly   = IndentationHint(insertSpaces = Some(false))
  private val widthOnly3 = IndentationHint(tabSize = Some(3))

  "A document nothing speaks for" should "be indented by the global settings" in {
    DocumentIndentation().effective(global) shouldBe theSameInstanceAs(global)
  }

  "A detected indentation" should "replace the global settings" in {
    DocumentIndentation(detected = twoSpaces).effective(global) shouldBe IndentationSettings(2, insertSpaces = true)
  }

  it should "leave the global width when it only knows tabs from spaces" in {
    DocumentIndentation(detected = tabsOnly).effective(IndentationSettings(tabSize = 6)) shouldBe
      IndentationSettings(tabSize = 6, insertSpaces = false)
  }

  "The sources" should "rank a choice over configuration over detection" in {
    val all = DocumentIndentation(chosen = eightTabs, configured = twoSpaces, detected = IndentationHint.of(global))
    all.effective(global) shouldBe IndentationSettings(8, insertSpaces = false)

    all.copy(chosen = IndentationHint.Empty).effective(global) shouldBe IndentationSettings(2, insertSpaces = true)
    all
      .copy(chosen = IndentationHint.Empty, configured = IndentationHint.Empty)
      .effective(
        IndentationSettings(tabSize = 8, insertSpaces = false)
      ) shouldBe global
  }

  it should "settle each field from the highest source that has it" in {
    val indentation = DocumentIndentation(configured = tabsOnly, detected = twoSpaces)

    indentation.effective(global) shouldBe IndentationSettings(tabSize = 2, insertSpaces = false)
    DocumentIndentation(configured = widthOnly3, detected = tabsOnly).effective(global) shouldBe
      IndentationSettings(tabSize = 3, insertSpaces = false)
  }

  it should "keep the width within what a tab size can be" in {
    DocumentIndentation(detected = IndentationHint(tabSize = Some(40))).effective(global).tabSize shouldBe 16
  }

  "Choosing an indentation" should "put it ahead of the other sources, and None hand the choice back" in {
    val detected = DocumentIndentation(detected = twoSpaces)

    val chosen = detected.withChosen(Some(IndentationSettings(tabSize = 8, insertSpaces = false)))
    chosen.effective(global) shouldBe IndentationSettings(8, insertSpaces = false)
    chosen.withChosen(None) shouldBe detected
  }

  "A session key" should "round-trip the settings it stands for" in {
    val settings = List(IndentationSettings(4), IndentationSettings(1), IndentationSettings(16, insertSpaces = false))

    settings.map(setting => IndentationSettings.fromConfigKey(IndentationSettings.configKey(setting))) shouldBe
      settings.map(Some(_))
    IndentationSettings.configKey(IndentationSettings(2, insertSpaces = false)) shouldBe "tabs:2"
  }

  it should "reject what is not a style and a width in range" in {
    List("", "spaces", "spaces:x", "spaces:0", "tabs:17", "mixed:4", "spaces:4:4").map(
      IndentationSettings.fromConfigKey
    ) shouldBe List.fill(7)(None)
  }

  "The status label" should "name spaces and tab size" in {
    IndentationSettings(4).statusLabel shouldBe "Spaces: 4"
    IndentationSettings(8, insertSpaces = false).statusLabel shouldBe "Tab Size: 8"
  }
