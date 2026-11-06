package com.serenity.command

import com.serenity.config.AppConfig
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontFamilyCatalog
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The font choices a runner shows are indexed into the catalog the runner holds, not into whatever fonts the machine
  * has installed.
  */
class CommandRunnerFontSelectionSpec extends AnyFlatSpec with Matchers:

  private val catalog = FontFamilyCatalog(
    monospace = List("Mono A", "Mono B", "Mono C"),
    text = List("Serif A", "Serif B"),
    ui = List("Sans A", "Sans B", "Sans C", "Sans D")
  )

  private val config = AppConfig.default.withFontConfig(
    FontLoader.FontConfig(codeFontFamily = "Mono C", textFontFamily = "Serif B", uiFontFamily = "Sans D")
  )

  private def runnerHolding(fonts: FontFamilyCatalog): CommandRunner =
    CommandRunner(isActive = false, fontFamilies = fonts)

  "An activated command runner" should "select the configured code, text and UI fonts in its own catalog" in {
    val selections = runnerHolding(catalog).activate(CommandRegistry.default, config).optionSelections

    (selections("code-font"), selections("text-font"), selections("ui-font")) shouldBe (2, 1, 3)
  }

  "A command runner given a new config" should "select the configured fonts in its own catalog" in {
    val selections = runnerHolding(catalog).updateInputItems(config).optionSelections

    (selections("code-font"), selections("text-font"), selections("ui-font")) shouldBe (2, 1, 3)
  }

  "A configured font the catalog does not list" should "select the first entry" in {
    val unlisted = AppConfig.default.withFontConfig(
      FontLoader.FontConfig(codeFontFamily = "Nowhere", textFontFamily = "Nowhere", uiFontFamily = "Nowhere")
    )
    val selections = runnerHolding(catalog).activate(CommandRegistry.default, unlisted).optionSelections

    (selections("code-font"), selections("text-font"), selections("ui-font")) shouldBe (0, 0, 0)
  }
