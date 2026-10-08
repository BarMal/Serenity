package com.serenity.ui.terminal

import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.KeyStroke

import com.serenity.command.menu.MenuAccelerators
import com.serenity.config.HotkeyTrigger
import com.serenity.keystroke.{InputKey, Modifier}

/** Turns a keymap trigger into the stroke a menu item shows, in the form `KeyStroke.getKeyStrokeForEvent` gives a key
  * press, so the activation guard can compare the two.
  */
object MenuKeyStrokes:

  def of(trigger: HotkeyTrigger): Option[KeyStroke] =
    Option
      .when(MenuAccelerators.isRepresentable(trigger))(keyCode(trigger))
      .flatten
      .filter(_ != KeyEvent.VK_UNDEFINED)
      .map(code => KeyStroke.getKeyStroke(code, modifierMask(trigger.modifiers)))

  private def modifierMask(modifiers: Set[Modifier]): Int =
    modifiers.foldLeft(0): (mask, modifier) =>
      mask | (modifier match
        case Modifier.Ctrl  => InputEvent.CTRL_DOWN_MASK
        case Modifier.Alt   => InputEvent.ALT_DOWN_MASK
        case Modifier.Shift => InputEvent.SHIFT_DOWN_MASK
        case Modifier.Meta  => InputEvent.META_DOWN_MASK)

  /** F10 is left out: the Metal look and feel binds it to the menu bar, so showing it would promise a key that opens
    * the menu instead.
    */
  private def keyCode(trigger: HotkeyTrigger): Option[Int] =
    trigger.keyType match
      case InputKey.Character  => trigger.character.map(char => KeyEvent.getExtendedKeyCodeForChar(char.toInt))
      case InputKey.Enter      => Some(KeyEvent.VK_ENTER)
      case InputKey.Backspace  => Some(KeyEvent.VK_BACK_SPACE)
      case InputKey.Delete     => Some(KeyEvent.VK_DELETE)
      case InputKey.Escape     => Some(KeyEvent.VK_ESCAPE)
      case InputKey.Tab        => Some(KeyEvent.VK_TAB)
      case InputKey.ArrowUp    => Some(KeyEvent.VK_UP)
      case InputKey.ArrowDown  => Some(KeyEvent.VK_DOWN)
      case InputKey.ArrowLeft  => Some(KeyEvent.VK_LEFT)
      case InputKey.ArrowRight => Some(KeyEvent.VK_RIGHT)
      case InputKey.Home       => Some(KeyEvent.VK_HOME)
      case InputKey.End        => Some(KeyEvent.VK_END)
      case InputKey.PageUp     => Some(KeyEvent.VK_PAGE_UP)
      case InputKey.PageDown   => Some(KeyEvent.VK_PAGE_DOWN)
      case InputKey.F1         => Some(KeyEvent.VK_F1)
      case InputKey.F2         => Some(KeyEvent.VK_F2)
      case InputKey.F3         => Some(KeyEvent.VK_F3)
      case InputKey.F4         => Some(KeyEvent.VK_F4)
      case InputKey.F5         => Some(KeyEvent.VK_F5)
      case InputKey.F6         => Some(KeyEvent.VK_F6)
      case InputKey.F7         => Some(KeyEvent.VK_F7)
      case InputKey.F8         => Some(KeyEvent.VK_F8)
      case InputKey.F9         => Some(KeyEvent.VK_F9)
      case InputKey.F11        => Some(KeyEvent.VK_F11)
      case InputKey.F12        => Some(KeyEvent.VK_F12)
      case _                   => None
