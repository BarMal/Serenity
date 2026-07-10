package com.serenity

import com.serenity.animation.AnimationConfig
import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorConfigSpec extends AnyFlatSpec with Matchers:

  "EditorConfig" should "own character animation and font schema metadata" in {
    EditorConfig.Schema.currentKeys.should(
      contain allOf (
        "character.animation",
        "character.animation.duration_ms",
        "character.animation.steps",
        "font.code.family",
        "font.text.family",
        "font.ui.family",
        "font.code.size",
        "font.text.size",
        "font.ui.size",
        "font.scale.mode",
        "font.text_scale",
        "font.code.ligatures",
        "font.text.ligatures",
        "font.ui.ligatures"
      )
    )

    EditorConfig.Schema.deprecatedKeys.should(
      contain allOf (
        "character_animation"             -> "character.animation",
        "character_animation_duration_ms" -> "character.animation.duration_ms",
        "font_code_family"                -> "font.code.family",
        "font_size"                       -> "font.code.size and font.text.size",
        "font_ui_ligatures"               -> "font.ui.ligatures"
      )
    )
  }

  it should "group editor animation, fonts, and pane width under AppConfig" in {
    val animation = AnimationConfig.custom(durationMs = 240)
    val fonts = FontConfig(
      codeFontFamily = "Monospaced",
      textFontFamily = "Serif",
      uiFontFamily = "Dialog",
      fontSize = 14.0f,
      textFontSize = 18.0f,
      uiFontSize = 13.0f,
      enableLigatures = false,
      textLigatures = true,
      uiLigatures = false
    )

    val config = AppConfig.default
      .withCharacterAnimation(animation.get)
      .withFontConfig(fonts)
      .withMinimumPaneWidth(72)

    config.editorConfig.characterAnimation.shouldBe(animation)
    config.editorConfig.fontConfig.shouldBe(fonts)
    config.editorConfig.minimumPaneWidth.shouldBe(72)
  }
