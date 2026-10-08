package com.serenity.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HotkeyTriggerLabelSpec extends AnyFlatSpec with Matchers:

  private def label(binding: String, osName: String): String =
    HotkeyTrigger.parse(binding).map(_.label(osName)).getOrElse(fail(s"unparseable binding: $binding"))

  "HotkeyTrigger.label" should "show the meta modifier as Cmd on macOS" in {
    label("meta+p", "Mac OS X") shouldBe "Cmd+P"
    label("meta+shift+f", "Mac OS X") shouldBe "Cmd+Shift+F"
  }

  it should "show the meta modifier as Meta elsewhere" in {
    label("meta+p", "Linux") shouldBe "Meta+P"
  }

  it should "capitalise modifiers and named keys" in {
    label("ctrl+shift+f", "Linux") shouldBe "Ctrl+Shift+F"
    label("alt+left", "Linux") shouldBe "Alt+Left"
    label("ctrl+tab", "Windows 11") shouldBe "Ctrl+Tab"
  }

  it should "spell page keys and end of input as words" in {
    label("ctrl+shift+pageup", "Linux") shouldBe "Ctrl+Shift+PageUp"
    label("ctrl+shift+pagedown", "Linux") shouldBe "Ctrl+Shift+PageDown"
    label("eof", "Linux") shouldBe "EOF"
  }

  it should "keep a plus key as a plus" in {
    label("ctrl+=", "Linux") shouldBe "Ctrl+="
    HotkeyTrigger(com.serenity.keystroke.InputKey.Character, Some('+'), Set(com.serenity.keystroke.Modifier.Ctrl))
      .label("Linux") shouldBe "Ctrl++"
  }

  it should "render a double-tap of a lone modifier" in {
    label("ctrl+ctrl", "Linux") shouldBe "Ctrl+Ctrl"
  }
