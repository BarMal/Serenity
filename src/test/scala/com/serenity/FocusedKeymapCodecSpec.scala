package com.serenity

import com.serenity.config.HotkeyConfig.given
import com.serenity.config.{CommandRunnerKeyAction, HotkeyTrigger, KeymapGroupConfig}
import com.serenity.keystroke.{InputKey, Modifier}
import com.serenity.keystroke.events.CommandRunnerEvent
import _root_.io.circe.Json
import _root_.io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A saved preset written by another build can carry keymap action keys this build no longer defines (e.g.
  * `next_category`). Decoding must skip the unknown key and keep the recognized bindings, rather than failing the whole
  * keymap section and silently dropping the user's customized keymap back to defaults (which also spammed a decode
  * warning per preset).
  */
class FocusedKeymapCodecSpec extends AnyFlatSpec with Matchers:

  "KeymapGroupConfig decoding" should "skip unknown action keys and keep the recognized bindings" in {
    val json = Json.obj(
      "submit"        -> List(HotkeyTrigger(keyType = InputKey.Enter, character = None, modifiers = Set(Modifier.Ctrl))).asJson,
      "next_category" -> List(HotkeyTrigger(keyType = InputKey.Tab, character = None, modifiers = Set.empty)).asJson
    )

    val decoded = json.as[KeymapGroupConfig[CommandRunnerKeyAction, CommandRunnerEvent]]

    val group = decoded.getOrElse(fail(s"keymap group should decode despite the unknown key, got: $decoded"))
    group.bindingsFor(CommandRunnerKeyAction.Submit).map(_.render) shouldBe List("ctrl+enter")
    // Recognized actions the JSON didn't override remain present, merged onto defaults.
    group.bindingsFor(CommandRunnerKeyAction.Dismiss).map(_.render) shouldBe List("escape")
  }
