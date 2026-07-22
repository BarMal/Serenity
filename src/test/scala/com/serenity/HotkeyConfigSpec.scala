package com.serenity

import com.serenity.config.{HotkeyAction, HotkeyConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HotkeyConfigSpec extends AnyFlatSpec with Matchers:

  private val editingActions = List(
    HotkeyAction.Find,
    HotkeyAction.Replace,
    HotkeyAction.GoToLine,
    HotkeyAction.SaveAs
  )

  "HotkeyConfig" should "supply conventional Ctrl defaults for core editing workflows on Windows and Linux" in {
    val bindings = HotkeyConfig.defaultBindingsFor("Linux")

    editingActions.map(action => bindings(action).head.render) shouldBe List(
      "ctrl+f",
      "ctrl+h",
      "ctrl+g",
      "ctrl+shift+s"
    )
  }

  it should "supply Command defaults for core editing workflows on macOS" in {
    val bindings = HotkeyConfig.defaultBindingsFor("Mac OS X")

    editingActions.map(action => bindings(action).head.render) shouldBe List(
      "meta+f",
      "meta+h",
      "meta+g",
      "meta+shift+s"
    )
  }

  it should "preserve a user override over its platform default" in {
    val config = HotkeyConfig.forOs("Mac OS X").withBinding(HotkeyAction.Find, "ctrl+alt+f")

    config.bindingsFor(HotkeyAction.Find).map(_.render) shouldBe List("ctrl+alt+f")
  }

  it should "report conflicting bindings during validation" in {
    val bindings = Map(
      HotkeyAction.Find -> HotkeyConfig.defaultBindingsFor("Linux")(HotkeyAction.Find),
      HotkeyAction.Replace -> HotkeyConfig.defaultBindingsFor("Linux")(HotkeyAction.Find)
    )

    HotkeyConfig.validate(bindings).isLeft shouldBe true
  }
