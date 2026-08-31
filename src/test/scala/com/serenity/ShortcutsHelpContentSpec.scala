package com.serenity

import com.serenity.config.{AppConfig, EditorKeyAction, HotkeyAction}
import com.serenity.state.models.ShortcutsHelpContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShortcutsHelpContentSpec extends AnyFlatSpec with Matchers:

  "ShortcutsHelpContent.build" should "produce one entry per bound global hotkey action, sourced from the real config" in {
    val groups = ShortcutsHelpContent.build(AppConfig.default)
    val global = groups.find(_.title == "Global").getOrElse(fail("Expected a Global group"))

    val saveEntry = global.entries.find(_.label == "Save").getOrElse(fail("Expected a Save entry"))
    saveEntry.keys shouldBe AppConfig.default.inputConfig.hotkeyConfig
      .bindingsFor(HotkeyAction.Save)
      .map(_.render)
      .mkString(" / ")
  }

  it should "include a rebound action under its new keys, not its stale default" in {
    val rebound   = AppConfig.default.withHotkeyOverrideUnbindingConflicts(HotkeyAction.Save, "ctrl+alt+k")
    val groups    = ShortcutsHelpContent.build(rebound)
    val global    = groups.find(_.title == "Global").getOrElse(fail("Expected a Global group"))
    val saveEntry = global.entries.find(_.label == "Save").getOrElse(fail("Expected a Save entry"))

    saveEntry.keys shouldBe "ctrl+alt+k"
  }

  it should "carry the Toggle Shortcuts Help action's own binding (F1 by default)" in {
    val groups = ShortcutsHelpContent.build(AppConfig.default)
    val global = groups.find(_.title == "Global").getOrElse(fail("Expected a Global group"))

    global.entries.find(_.label == "Toggle Shortcuts Help").map(_.keys) shouldBe Some("f1")
  }

  it should "include an Editor group covering the editor's own movement keymap" in {
    val groups = ShortcutsHelpContent.build(AppConfig.default)
    val editor = groups.find(_.title == "Editor").getOrElse(fail("Expected an Editor group"))

    editor.entries.map(_.label) should contain("Move Left")
    val moveLeft = editor.entries.find(_.label == "Move Left").get
    moveLeft.keys shouldBe AppConfig.default.inputConfig.focusedKeymapConfig.editor
      .bindingsFor(EditorKeyAction.MoveLeft)
      .map(_.render)
      .mkString(" / ")
  }

  it should "never return an empty group" in
    ShortcutsHelpContent.build(AppConfig.default).foreach(_.entries should not be empty)
