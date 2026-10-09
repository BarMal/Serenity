package com.serenity.config

import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The macOS delete shortcuts, and Shift left held after a capital, are ordinary default editor bindings. They are
  * unconditional rather than per platform: a terminal never reports Meta, and on Windows and Linux nothing else binds
  * Meta+Backspace, Meta+Delete or Alt+Delete, so the same triggers are inert there.
  */
class EditorDeleteKeymapSpec extends AnyFlatSpec with Matchers:

  private def trigger(key: InputKey, modifier: Modifier*) = HotkeyTrigger(key, None, modifier.toSet)

  private def bound(action: EditorKeyAction) = EditorKeyAction.defaultBindings(action)

  "Default editor bindings" should "delete to the line start on Cmd+Backspace" in {
    bound(EditorKeyAction.DeleteToLineStart) shouldBe List(trigger(InputKey.Backspace, Modifier.Meta))
  }

  it should "delete to the line end on Cmd+Delete" in {
    bound(EditorKeyAction.DeleteToLineEnd) shouldBe List(trigger(InputKey.Delete, Modifier.Meta))
  }

  it should "delete the word forward on Option+Delete as well as Ctrl+Delete" in {
    bound(EditorKeyAction.DeleteWordForward) should contain allOf (
      trigger(InputKey.Delete, Modifier.Ctrl),
      trigger(InputKey.Delete, Modifier.Alt)
    )
  }

  it should "treat Shift+Backspace as Backspace" in {
    bound(EditorKeyAction.DeleteBackward) should contain allOf (
      trigger(InputKey.Backspace),
      trigger(InputKey.Backspace, Modifier.Shift)
    )
  }

  it should "treat Shift+Delete as Delete" in {
    bound(EditorKeyAction.DeleteForward) should contain allOf (
      trigger(InputKey.Delete),
      trigger(InputKey.Delete, Modifier.Shift)
    )
  }
