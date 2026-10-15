package com.serenity.config

import java.nio.file.Files

import com.serenity.TestTemp
import com.serenity.keystroke.Modifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SummonConfigFileSpec extends AnyFlatSpec with Matchers:

  private def loadResult(text: String) =
    val file = TestTemp.file("serenity-summon-config", ".conf")
    Files.writeString(file, text)
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  private val keys = List(
    "ui.summon.modifiers",
    "ui.summon.lock",
    "ui.summon.line_numbers",
    "ui.summon.status_bar",
    "ui.summon.tab_strip",
    "ui.summon.scrollbar",
    "ui.summon.pane_headers"
  )

  "The summon settings" should "ship as Alt+Shift, Space to lock, and L, S, T, B, H" in {
    AppConfig.default.summon shouldBe SummonConfig()
    keys.map(key => ConfigRegistry.defaultFor(key).map(_.rendered)) shouldBe
      List("\"alt+shift\"", "space", "l", "s", "t", "b", "h").map(Some(_))
  }

  it should "all be known keys" in
    keys.foreach(key => withClue(key)(ConfigKeySchema.isKnownKey(key) shouldBe true))

  it should "reject a value they cannot read" in {
    ConfigRegistry.rejects("ui.summon.modifiers", "alt+shift") shouldBe false
    ConfigRegistry.rejects("ui.summon.modifiers", "alt+l") shouldBe true
    ConfigRegistry.rejects("ui.summon.modifiers", "") shouldBe true
    ConfigRegistry.rejects("ui.summon.lock", "enter") shouldBe false
    ConfigRegistry.rejects("ui.summon.lock", "ctrl+space") shouldBe true
    ConfigRegistry.rejects("ui.summon.scrollbar", "off") shouldBe false
    ConfigRegistry.rejects("ui.summon.scrollbar", "x") shouldBe false
    ConfigRegistry.rejects("ui.summon.scrollbar", "xx") shouldBe true
  }

  it should "load from the config file and be written back" in {
    val result = loadResult(
      """ui.summon.modifiers = "ctrl+alt"
        |ui.summon.lock = enter
        |ui.summon.line_numbers = x
        |ui.summon.scrollbar = off
        |""".stripMargin
    )
    val summon = result.config.summon

    summon.modifiers shouldBe Set(Modifier.Ctrl, Modifier.Alt)
    summon.lock shouldBe SummonConfig.parseLock("enter").getOrElse(fail("enter parses"))
    summon.letterOf(ChromeElement.LineNumbers) shouldBe Some('x')
    summon.letterOf(ChromeElement.Scrollbar) shouldBe None
    summon.letterOf(ChromeElement.StatusBar) shouldBe Some('s')
    result.report.invalidEntries shouldBe Nil

    val written = ConfigManager.configToString(result.config)
    written should include("ui.summon.modifiers = \"ctrl+alt\"")
    written should include("ui.summon.lock = enter")
    written should include("ui.summon.line_numbers = x")
    written should include("ui.summon.scrollbar = off")
  }

  it should "report an invalid value through the invalid-entry report and keep the default" in {
    val result = loadResult("ui.summon.modifiers = \"banana\"\nui.summon.lock = ctrl+x\nui.summon.tab_strip = tt\n")

    result.report.invalidEntries.map(_.key) should contain allOf (
      "ui.summon.modifiers",
      "ui.summon.lock",
      "ui.summon.tab_strip"
    )
    result.config.summon shouldBe SummonConfig()
  }

  "A summon chord a hotkey already holds" should "go to the hotkey, and be reported like any hotkey conflict" in {
    val result = loadResult("""hotkey.find = ["alt+shift+l"]""" + "\n")

    result.config.summon.letterOf(ChromeElement.LineNumbers) shouldBe None
    result.config.summon.letterOf(ChromeElement.StatusBar) shouldBe Some('s')
    result.report.hotkeyConflicts shouldBe
      List(HotkeyConflict("alt+shift+l", "find", List("ui.summon.line_numbers")))
    result.config.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.Find).map(_.render) shouldBe List("alt+shift+l")
  }

  it should "not be reported when the base chord has been moved off it" in {
    val result = loadResult("""hotkey.find = ["alt+shift+l"]""" + "\nui.summon.modifiers = \"ctrl+alt\"\n")

    result.report.hotkeyConflicts shouldBe Nil
    result.config.summon.letterOf(ChromeElement.LineNumbers) shouldBe Some('l')
  }

  "Two elements given one letter" should "leave it to the first" in {
    val result = loadResult("ui.summon.scrollbar = l\n")

    result.config.summon.letterOf(ChromeElement.LineNumbers) shouldBe Some('l')
    result.config.summon.letterOf(ChromeElement.Scrollbar) shouldBe None
    result.report.hotkeyConflicts.map(_.droppedFrom) shouldBe List(List("ui.summon.scrollbar"))
  }
