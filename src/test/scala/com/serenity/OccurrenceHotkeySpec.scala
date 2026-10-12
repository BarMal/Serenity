package com.serenity

import com.serenity.command.{CommandIntent, CommandKeyBindings, CommandRegistry, EditIntent}
import com.serenity.config.{HotkeyAction, HotkeyConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Ctrl+D selects the next occurrence, as in VS Code and Sublime Text, so pane splitting gave up that key (#1994). */
class OccurrenceHotkeySpec extends AnyFlatSpec with Matchers:

  private val platforms = List("Linux", "Windows", "Mac OS X")

  private def primary(osName: String): String = if osName == "Mac OS X" then "meta" else "ctrl"

  // `HotkeyTrigger.render` lists modifiers as ctrl, alt, meta, shift.
  private def withAlt(osName: String, key: Char): String =
    if osName == "Mac OS X" then s"alt+meta+$key" else s"ctrl+alt+$key"

  private def commandKeys(osName: String, commandId: String): List[String] =
    HotkeyConfig.forOs(osName).commandBindingsFor(commandId).map(_.render)

  "Select Next Occurrence" should "be bound to Ctrl+D (Cmd+D on macOS)" in
    platforms.foreach(os => commandKeys(os, "select-next-occurrence") shouldBe List(s"${primary(os)}+d"))

  "Skip Occurrence" should "be bound to Ctrl+Alt+K (Cmd+Alt+K on macOS), the single-key stand-in for Ctrl+K Ctrl+D" in
    platforms.foreach(os => commandKeys(os, "skip-occurrence") shouldBe List(withAlt(os, 'k')))

  "Select All Occurrences" should "be bound to Ctrl+Shift+L (Cmd+Shift+L on macOS)" in
    platforms.foreach(os => commandKeys(os, "select-all-occurrences") shouldBe List(s"${primary(os)}+shift+l"))

  "Split Pane Horizontal" should "have moved to Ctrl+Alt+V (Cmd+Alt+V on macOS), leaving Ctrl+D to occurrences" in
    platforms.foreach { os =>
      HotkeyConfig
        .defaultBindingsFor(os)(HotkeyAction.SplitPaneHorizontal)
        .map(_.render) shouldBe List(withAlt(os, 'v'))
    }

  "Toggle Notes Pin" should "have moved to Ctrl+Alt+P (Cmd+Alt+P on macOS), leaving Ctrl+Shift+L to occurrences" in
    platforms.foreach { os =>
      HotkeyConfig
        .defaultBindingsFor(os)(HotkeyAction.ToggleNotesPin)
        .map(_.render) shouldBe List(withAlt(os, 'p'))
    }

  "The default keymap" should "hold no key twice, across actions and commands, on every platform" in
    platforms.foreach(os => HotkeyConfig.validate(HotkeyConfig.forOs(os)) shouldBe Right(()))

  it should "survive the terminal rewrite of the macOS defaults without a clash" in {
    HotkeyConfig.validate(HotkeyConfig.forOs("Mac OS X").forTerminalUse) shouldBe Right(())
  }

  "The palette" should "offer the three occurrence commands as edit intents, after every other command" in {
    val commands = CommandRegistry.default.getAllCommands
    commands.takeRight(3).map(_.name) shouldBe List(
      "select-next-occurrence",
      "skip-occurrence",
      "select-all-occurrences"
    )
    commands.takeRight(3).map(_.intent) shouldBe List(
      CommandIntent.Edit(EditIntent.SelectNextOccurrence),
      CommandIntent.Edit(EditIntent.SkipOccurrence),
      CommandIntent.Edit(EditIntent.SelectAllOccurrences)
    )
  }

  it should "show each command's key beside it" in {
    val shown = CommandKeyBindings.displayed(HotkeyConfig.forOs("Linux"))

    shown.get("select-next-occurrence") shouldBe Some("ctrl+d")
    shown.get("select-all-occurrences") shouldBe Some("ctrl+shift+l")
    shown.get("skip-occurrence") shouldBe Some("ctrl+alt+k")
  }
