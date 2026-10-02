package com.serenity.config

import java.nio.file.Files

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PanelEscapeTargetConfigSpec extends AnyFlatSpec with Matchers:

  private val codeKey  = "ui.panel.escape_returns_to.code"
  private val proseKey = "ui.panel.escape_returns_to.prose"

  "PerMode" should "hold one value per app mode" in {
    val values = PerMode(code = 1, prose = 2)

    values.forMode(AppMode.Code) shouldBe 1
    values.forMode(AppMode.Prose) shouldBe 2
  }

  it should "replace only the value for the mode it is given" in {
    val values = PerMode.both("a")

    values.updated(AppMode.Prose, "b") shouldBe PerMode(code = "a", prose = "b")
    values.updated(AppMode.Code, "c") shouldBe PerMode(code = "c", prose = "a")
  }

  "Escape from a focused panel" should "return to the editor in both modes by default" in {
    AppConfig.default.inputConfig.panelEscapeReturnsTo shouldBe PerMode.both(PanelEscapeTarget.Editor)
    ConfigRegistry.defaultFor(codeKey).map(_.rendered) shouldBe Some("editor")
    ConfigRegistry.defaultFor(proseKey).map(_.rendered) shouldBe Some("editor")
  }

  it should "read each mode's target from its own key" in {
    val config = ConfigRegistry.read(AppConfig.default, proseKey, "previous").getOrElse(fail(s"$proseKey unread"))

    config.inputConfig.panelEscapeReturnsTo shouldBe
      PerMode(code = PanelEscapeTarget.Editor, prose = PanelEscapeTarget.Previous)
  }

  it should "reject a value that is not a target" in {
    ConfigRegistry.rejects(codeKey, "sideways") shouldBe true
    ConfigRegistry.rejects(codeKey, "previous") shouldBe false
  }

  it should "resolve to the current app mode's target" in {
    val config = AppConfig.default.withPanelEscapeTarget(AppMode.Prose, PanelEscapeTarget.Previous)

    config.withAppMode(AppMode.Code).panelEscapeTarget shouldBe PanelEscapeTarget.Editor
    config.withAppMode(AppMode.Prose).panelEscapeTarget shouldBe PanelEscapeTarget.Previous
  }

  it should "survive a save and reload of the config file" in {
    val config = AppConfig.default
      .withPanelEscapeTarget(AppMode.Code, PanelEscapeTarget.Previous)
      .withPanelEscapeTarget(AppMode.Prose, PanelEscapeTarget.Editor)
    val file = Files.createTempFile("serenity-panel-escape", ".conf")
    try
      ConfigManagerTestSupport.saveConfig(config, file) shouldBe true
      Files.readString(file) should include(s"$codeKey = previous")
      ConfigManagerTestSupport.loadConfig(Some(file.toString)).inputConfig.panelEscapeReturnsTo shouldBe
        PerMode(code = PanelEscapeTarget.Previous, prose = PanelEscapeTarget.Editor)
    finally Files.deleteIfExists(file): Unit
  }
