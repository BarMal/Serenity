package com.serenity.command

import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPreset
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PresetSettingsEditSpec extends AnyFlatSpec with Matchers:

  private val preset = UiPreset("Drafting", AppConfig.default.withInterfaceDensity(InterfaceDensity.Compact))

  private def settings(intent: SettingsIntent): CommandIntent = CommandIntent.Settings(intent)

  private def edited(intent: CommandIntent): UiPreset =
    PresetSettingsEdit(intent, preset).getOrElse(fail(s"a preset should hold $intent"))

  "PresetSettingsEdit" should "set a font size on the preset's font config only" in {
    val result = edited(settings(SettingsIntent.Font(FontIntent.SetCodeFontSize(21.0f))))

    result.config.editorConfig.fontConfig shouldBe preset.config.editorConfig.fontConfig.copy(fontSize = 21.0f)
    result.config.withFontConfig(preset.config.editorConfig.fontConfig) shouldBe preset.config
  }

  it should "clamp a font size the way the live setting does" in {
    val result = edited(settings(SettingsIntent.Font(FontIntent.SetCodeFontSize(500.0f))))

    result.config.editorConfig.fontConfig.codeFontSize shouldBe 48.0f
  }

  it should "set the default document mode, Markdown view and drop caps on the document config" in {
    edited(
      CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText))
    ).config.documentConfig.defaultMode shouldBe DefaultDocumentMode.RichText
    edited(
      CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.InlineLens))
    ).config.documentConfig.markdownViewMode shouldBe MarkdownViewMode.InlineLens
    edited(
      settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetDropCapsEnabled(false)))
    ).config.documentConfig.dropCapsEnabled shouldBe false
  }

  it should "set spell-check words without touching the rest of the spell-check setup" in {
    val result = edited(settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckWords(List("nocturne")))))

    result.config.languageToolsConfig.spellCheck shouldBe
      preset.config.languageToolsConfig.spellCheck.copy(additionalWords = List("nocturne"))
  }

  it should "leave the rest of the preset as it was" in {
    val result = edited(settings(SettingsIntent.Font(FontIntent.SetUiFontSize(20.0f))))

    result.copy(config = preset.config) shouldBe preset
    result.config.interfaceConfig shouldBe preset.config.interfaceConfig
  }

  it should "refuse a setting a preset does not hold" in {
    PresetSettingsEdit(
      settings(SettingsIntent.General(GeneralSettingsIntent.SetAutoSaveMode(AutoSaveMode.Off))),
      preset
    ) shouldBe None
    PresetSettingsEdit(CommandIntent.View(ViewIntent.ArrangePanels), preset) shouldBe None
    PresetSettingsEdit(
      settings(SettingsIntent.SpellCheck(SpellCheckIntent.AddWordAtCursorToDictionary)),
      preset
    ) shouldBe None
  }

  "FontConfigEdit" should "apply the same edit to a font config as the live setting" in {
    FontConfigEdit(FontIntent.ToggleLigatures)(FontConfig(enableLigatures = true, textLigatures = true)) shouldBe
      FontConfig(enableLigatures = false, textLigatures = false)
  }
