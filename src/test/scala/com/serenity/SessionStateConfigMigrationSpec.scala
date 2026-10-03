package com.serenity

import java.awt.Font

import _root_.io.circe.Json
import _root_.io.circe.syntax.*
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

  // issue tracked by the elementGap Option conversion: unset now means "surface-resolved default", not a baked-in
  // zero, so an older session file that never wrote this key must decode to `None` -- not to either surface's
  // number -- exactly as if the field had never existed on that file's `AppConfig` at all.
  it should "leave UI element gap unset when loading older JSON without the field" in {
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
    decoded.toOption.get.config.uiElementGap shouldBe None
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

  it should "default the status segments when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutCursorInfoBarSegments =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("statusSegments")))
      )

    val decoded = jsonWithoutCursorInfoBarSegments.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.statusLine.segments shouldBe StatusLineConfig.defaultSegments
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
            configObject
              .remove("statusSegments")
              .remove("statusPlacement")
              .add("cursorInfoBarMode", Json.fromString("detailed"))
          )
        )
      )

    val decoded = legacyJson.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.statusLine.segments shouldBe
      List(StatusSegment.Position, StatusSegment.Title, StatusSegment.Mode)
  }

  it should "default the status placement to pinned when loading older JSON without the field" in {
    val originalJson = SessionState
      .fromAppState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = AppConfig.default)))
      .asJson
    val configObject =
      originalJson.hcursor.downField("config").focus.flatMap(_.asObject).getOrElse(fail("Expected config object"))
    val jsonWithoutStatusLinePlacement =
      originalJson.mapObject(
        _.add("config", _root_.io.circe.Json.fromJsonObject(configObject.remove("statusPlacement")))
      )

    val decoded = jsonWithoutStatusLinePlacement.as[SessionState]

    decoded.isRight shouldBe true
    decoded.toOption.get.config.statusLine.placement shouldBe StatusLinePlacement.Pinned
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
