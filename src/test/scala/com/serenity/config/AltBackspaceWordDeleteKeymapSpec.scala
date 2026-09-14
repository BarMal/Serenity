package com.serenity.config

import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Ctrl+Backspace is the primary delete-word-backward binding, but many legacy terminals (Git Bash/MSYS) collapse it to
  * a plain Backspace (#1320). Alt+Backspace (ESC+DEL) is the readline-standard alternate that those terminals do
  * deliver, and `TerminalInputDecoder` decodes ESC+0x7f to Backspace+Alt -- so this alternate binding is reachable.
  */
class AltBackspaceWordDeleteKeymapSpec extends AnyFlatSpec with Matchers:

  private val altBackspace = HotkeyTrigger(InputKey.Backspace, None, Set(Modifier.Alt))

  "Default keymaps" should "bind Alt+Backspace to delete-word-backward in the editor" in {
    EditorKeyAction.defaultBindings(EditorKeyAction.DeleteWordBackward) should contain(altBackspace)
  }

  it should "bind Alt+Backspace to delete-word-backward in the command runner" in {
    CommandRunnerKeyAction.defaultBindings(CommandRunnerKeyAction.DeleteWordBackward) should contain(altBackspace)
  }

  it should "bind Alt+Backspace to delete-word-backward in modal text fields" in {
    ModalKeyAction.defaultBindings(ModalKeyAction.DeleteWordBackward) should contain(altBackspace)
  }
