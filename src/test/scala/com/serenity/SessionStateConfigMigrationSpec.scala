package com.serenity

import java.awt.Font

import _root_.io.circe.Json
import _root_.io.circe.syntax.*
import com.serenity.animation.AnimationConfig
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.session.given
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SessionStateConfigMigrationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "SessionState" should "default spell-check dictionary paths when loading older JSON without the field" in {
    val config = AppConfig.default.withSpellCheck(
      SpellCheckConfig(enabled = true, languages = List("en"), additionalWords = List("serenity"))
    )
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = config)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val spellCheckObject =
      originalJson.hcursor
        .downField("config")
        .downField("spellCheck")
        .focus
        .flatMap(_.asObject)
        .getOrElse(fail("Expected spellCheck object"))
    val jsonWithoutDictionaryPaths =
      originalJson.mapObject(
        _.add(
          "config",
          _root_.io.circe.Json.fromJsonObject(
            configObject.add(
              "spellCheck",
              _root_.io.circe.Json.fromJsonObject(spellCheckObject.remove("dictionaryPaths"))
            )
          )
        )
      )

    val decoded = jsonWithoutDictionaryPaths.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.languageToolsConfig.spellCheck.dictionaryPaths shouldBe Nil
  }

  it should "default backgroundStyle to Frosted when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutBackgroundStyle =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("backgroundStyle")))
      )

    val decoded = jsonWithoutBackgroundStyle.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.surfaceConfig.backgroundStyle shouldBe BackgroundStyle.Frosted
  }

  it should "default material and motion presets when loading older JSON without the fields" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutPresets =
      originalJson.mapObject(
        _.add(
          "config",
          _root_.io.circe.Json.fromJsonObject(configObject.remove("materialPreset").remove("motionPreset"))
        )
      )

    val decoded = jsonWithoutPresets.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.surfaceConfig.materialPreset shouldBe MaterialPreset.Frosted
    decoded.toOption.get.config.surfaceConfig.motionPreset shouldBe MotionPreset.Smooth
  }

  it should "default element transition speed scale when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutSpeedScale =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("elementTransitionSpeedScale")))
      )

    val decoded = jsonWithoutSpeedScale.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.surfaceConfig.elementTransitionSpeedScale shouldBe 1.0
  }

  it should "default per-family animation speed scales when loading older JSON without the fields" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutFamilyScales =
      originalJson.mapObject(
        _.add(
          "config",
          _root_.io.circe.Json.fromJsonObject(
            configObject
              .remove("editorTextTransitionSpeedScale")
              .remove("commandRunnerTransitionSpeedScale")
              .remove("uiTransitionSpeedScale")
              .remove("cursorTransitionSpeedScale")
          )
        )
      )

    val decoded = jsonWithoutFamilyScales.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.surfaceConfig.editorTextTransitionSpeedScale shouldBe None
    decoded.toOption.get.config.surfaceConfig.commandRunnerTransitionSpeedScale shouldBe None
    decoded.toOption.get.config.surfaceConfig.uiTransitionSpeedScale shouldBe None
    decoded.toOption.get.config.surfaceConfig.cursorTransitionSpeedScale shouldBe None
  }

  it should "default command and panel transition kind overrides when loading older JSON without the fields" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutPanelKinds =
      originalJson.mapObject(
        _.add(
          "config",
          _root_.io.circe.Json.fromJsonObject(
            configObject
              .remove("panelOpenTransitionKind")
              .remove("panelCloseTransitionKind")
              .remove("commandRunnerTransitionKind")
          )
        )
      )

    val decoded = jsonWithoutPanelKinds.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.surfaceConfig.panelOpenTransitionKind shouldBe None
    decoded.toOption.get.config.surfaceConfig.panelCloseTransitionKind shouldBe None
    decoded.toOption.get.config.surfaceConfig.commandRunnerTransitionKind shouldBe None
  }

  it should "default command runner animation when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutCommandRunnerAnimation =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("commandRunnerAnimation")))
      )

    val decoded = jsonWithoutCommandRunnerAnimation.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.surfaceConfig.commandRunnerAnimation shouldBe com.serenity.animation.AnimationConfig.smooth
  }

  it should "default UI animation when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutUiAnimation =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("uiAnimation")))
      )

    val decoded = jsonWithoutUiAnimation.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.surfaceConfig.uiAnimation shouldBe AppConfig.default.surfaceConfig.uiAnimation
  }

  it should "default renderFpsTarget to 60 FPS when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutRenderFps =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("renderFpsTarget")))
      )

    val decoded = jsonWithoutRenderFps.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.surfaceConfig.renderFpsTarget shouldBe RenderFpsTarget.Fps60
  }

  it should "default windowChromeMode to the app default when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutWindowChromeMode =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("windowChromeMode")))
      )

    val decoded = jsonWithoutWindowChromeMode.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.windowChromeMode shouldBe AppConfig.default.windowChromeMode
  }

  it should "default visualLineCursorNavigation to the app default when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutVisualLineCursorNavigation =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("visualLineCursorNavigation")))
      )

    val decoded = jsonWithoutVisualLineCursorNavigation.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.surfaceConfig.visualLineCursorNavigation shouldBe
      AppConfig.default.surfaceConfig.visualLineCursorNavigation
  }

  it should "default interfaceDensity to Comfortable when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutInterfaceDensity =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("interfaceDensity")))
      )

    val decoded = jsonWithoutInterfaceDensity.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.interfaceDensity shouldBe InterfaceDensity.Comfortable
  }

  it should "default UI element gap to zero when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutUiElementGap =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("uiElementGap")))
      )

    val decoded = jsonWithoutUiElementGap.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.uiElementGap shouldBe 0
  }

  it should "default UI corner radius to the existing panel radius when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutUiCornerRadius =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("uiCornerRadiusPx")))
      )

    val decoded = jsonWithoutUiCornerRadius.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.uiCornerRadiusPx shouldBe 8
  }

  it should "default UI outline thickness when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutUiOutlineThickness =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("uiOutlineThicknessPx")))
      )

    val decoded = jsonWithoutUiOutlineThickness.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.uiOutlineThicknessPx shouldBe 2
  }

  it should "default cursorInfoBarSegments to empty when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutCursorInfoBarSegments =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("cursorInfoBarSegments")))
      )

    val decoded = jsonWithoutCursorInfoBarSegments.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorInfoBarSegments shouldBe Nil
  }

  it should "restore a legacy session that stored a single cursorInfoBarMode instead of a segment list" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val legacyJson =
      originalJson.mapObject(
        _.add(
          "config",
          _root_.io.circe.Json.fromJsonObject(
            configObject.remove("cursorInfoBarSegments").add("cursorInfoBarMode", Json.fromString("detailed"))
          )
        )
      )

    val decoded = legacyJson.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorInfoBarSegments shouldBe
      List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title)
  }

  it should "default cursorInfoBarPlacement to Floating when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutCursorInfoBarPlacement =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("cursorInfoBarPlacement")))
      )

    val decoded = jsonWithoutCursorInfoBarPlacement.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.cursorInfoBarPlacement shouldBe CursorInfoBarPlacement.Floating
  }

  it should "default uiFontFamily to SansSerif when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val fontConfigObject = configObject("fontConfig").flatMap(_.asObject).getOrElse(fail("Expected fontConfig object"))
    val jsonWithoutUiFontFamily =
      originalJson.mapObject(
        _.add(
          "config",
          _root_.io.circe.Json.fromJsonObject(
            configObject.add(
              "fontConfig",
              _root_.io.circe.Json.fromJsonObject(fontConfigObject.remove("uiFontFamily"))
            )
          )
        )
      )

    val decoded = jsonWithoutUiFontFamily.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.editorConfig.fontConfig.uiFontFamily shouldBe Font.SANS_SERIF
  }
