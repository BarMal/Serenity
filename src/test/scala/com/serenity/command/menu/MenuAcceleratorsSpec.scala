package com.serenity.command.menu

import com.serenity.command.CommandId
import com.serenity.config.{HotkeyAction, HotkeyConfig, HotkeyTrigger}
import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuAcceleratorsSpec extends AnyFlatSpec with Matchers:

  private val mac   = HotkeyConfig.forOs("Mac OS X")
  private val linux = HotkeyConfig.forOs("Linux")

  private def shown(hotkeys: HotkeyConfig, id: String): Option[String] =
    MenuAccelerators.of(hotkeys).get(CommandId(id)).map(_.render)

  "Accelerators" should "follow the macOS defaults, leading with Cmd+Shift+Z for redo" in {
    shown(mac, "undo") shouldBe Some("meta+z")
    shown(mac, "redo") shouldBe Some("meta+shift+z")
    shown(mac, "save") shouldBe Some("meta+s")
  }

  it should "follow the other platforms' defaults, leading with Ctrl+Y for redo" in {
    shown(linux, "undo") shouldBe Some("ctrl+z")
    shown(linux, "redo") shouldBe Some("ctrl+y")
  }

  it should "skip a binding no menu can show, taking the next one" in {
    shown(linux, "quit") shouldBe Some("ctrl+q")
    val unshowableFirst = linux.copy(bindings =
      linux.bindings.updated(
        HotkeyAction.Save,
        List(HotkeyTrigger(InputKey.Ctrl, None, Set.empty), HotkeyTrigger(InputKey.Unknown, None, Set.empty)) ++
          linux.bindingsFor(HotkeyAction.Save)
      )
    )

    shown(unshowableFirst, "save") shouldBe Some("ctrl+s")
  }

  it should "reflect a binding the user overrode" in {
    shown(linux.withBinding(HotkeyAction.Undo, "ctrl+alt+u"), "undo") shouldBe Some("ctrl+alt+u")
  }

  it should "show nothing for a command with no keys" in {
    shown(linux.copy(bindings = linux.bindings.updated(HotkeyAction.Undo, Nil)), "undo") shouldBe None
  }

  it should "show the keys of the commands bound by id" in {
    shown(linux, "bold") shouldBe Some("ctrl+b")
    shown(mac, "bold") shouldBe Some("meta+b")
  }

  it should "show the shortcuts help key" in {
    shown(linux, "toggle-shortcuts-help") shouldBe
      linux.bindingsFor(HotkeyAction.ToggleShortcutsHelp).headOption.map(_.render)
    shown(linux, "toggle-shortcuts-help") should not be empty
  }

  "A representable trigger" should "be neither a bare modifier chord, nor EOF, Unknown or reverse tab" in {
    val character = HotkeyTrigger(InputKey.Character, Some('a'), Set(Modifier.Ctrl))

    MenuAccelerators.isRepresentable(character) shouldBe true
    MenuAccelerators.isRepresentable(HotkeyTrigger(InputKey.F1, None, Set.empty)) shouldBe true
    List(
      InputKey.Ctrl,
      InputKey.Meta,
      InputKey.Shift,
      InputKey.Alt,
      InputKey.EOF,
      InputKey.Unknown,
      InputKey.ReverseTab
    )
      .foreach(key => MenuAccelerators.isRepresentable(HotkeyTrigger(key, None, Set.empty)) shouldBe false)
  }
