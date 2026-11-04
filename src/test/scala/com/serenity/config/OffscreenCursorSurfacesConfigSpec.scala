package com.serenity.config

import java.nio.file.Files

import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `editor.cursor.offscreen_surfaces` chooses what a cursor-anchored surface does when it opens with the caret scrolled
  * out of the pane (#2570 item 3).
  */
class OffscreenCursorSurfacesConfigSpec extends AnyFlatSpec with Matchers:

  private val settingKey = "editor.cursor.offscreen_surfaces"

  private def loaded(content: String): ConfigLoadResult =
    val file = TestTemp.file("serenity-offscreen-surfaces", ".conf")
    Files.writeString(file, content)
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  private def mode(content: String): OffscreenCursorSurfaces =
    loaded(content).config.surfaceConfig.offscreenCursorSurfaces

  "The off-screen cursor surface setting" should "default to revealing the cursor" in {
    AppConfig.default.surfaceConfig.offscreenCursorSurfaces shouldBe OffscreenCursorSurfaces.RevealCursor
    ConfigRegistry.defaultFor(settingKey).map(_.rendered) shouldBe Some("reveal-cursor")
  }

  it should "be written to the reference config" in {
    ConfigRegistry.writtenKeys should contain(settingKey)
  }

  it should "read each of its three values" in {
    mode(s"$settingKey = reveal-cursor\n") shouldBe OffscreenCursorSurfaces.RevealCursor
    mode(s"$settingKey = summon-cursor\n") shouldBe OffscreenCursorSurfaces.SummonCursor
    mode(s"$settingKey = clamp\n") shouldBe OffscreenCursorSurfaces.Clamp
  }

  it should "ignore case and surrounding space" in {
    mode(s"$settingKey =  Clamp \n") shouldBe OffscreenCursorSurfaces.Clamp
  }

  it should "report an unknown value with the accepted choices and keep the default" in {
    val result = loaded(s"$settingKey = hide\n")
    val entry  = result.report.invalidEntries.find(_.key == settingKey)
    entry.map(_.value) shouldBe Some("hide")
    entry.map(_.reason).getOrElse("") should include("reveal-cursor, summon-cursor or clamp")
    result.config.surfaceConfig.offscreenCursorSurfaces shouldBe OffscreenCursorSurfaces.RevealCursor
  }

  "OffscreenCursorSurfaces.fromConfigKey" should "name each choice by its settingKey" in {
    OffscreenCursorSurfaces.values.toList.map(choice => OffscreenCursorSurfaces.fromConfigKey(choice.configKey)) shouldBe
      OffscreenCursorSurfaces.values.toList.map(Some(_))
  }
