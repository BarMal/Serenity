package com.serenity.input

import java.awt.event.KeyEvent

import com.serenity.input.SummonChordTracker.{RawKey, Symbol}
import com.serenity.keystroke.{InputKey, Modifier}

/** What [[SummonChordTracker]] needs to know of an AWT key event. Keys are named by their `VK_` code, so the chord
  * follows the keyboard layout's own letters rather than the character the modifiers turn them into.
  */
private[input] object SummonKeys:

  def rawKey(event: KeyEvent): RawKey = RawKey(event.getKeyCode, symbolOf(event.getKeyCode))

  def modifiersOf(event: KeyEvent): Set[Modifier] =
    Set(
      Option.when(event.isControlDown)(Modifier.Ctrl),
      Option.when(event.isAltDown)(Modifier.Alt),
      Option.when(event.isShiftDown)(Modifier.Shift),
      Option.when(event.isMetaDown)(Modifier.Meta)
    ).flatten

  private def symbolOf(code: Int): Symbol =
    code match
      case KeyEvent.VK_CONTROL => Symbol.Mod(Modifier.Ctrl)
      case KeyEvent.VK_ALT     => Symbol.Mod(Modifier.Alt)
      case KeyEvent.VK_SHIFT   => Symbol.Mod(Modifier.Shift)
      case KeyEvent.VK_META    => Symbol.Mod(Modifier.Meta)
      case letter if letter >= KeyEvent.VK_A && letter <= KeyEvent.VK_Z =>
        Symbol.Glyph((letter - KeyEvent.VK_A + 'a').toChar)
      case digit if digit >= KeyEvent.VK_0 && digit <= KeyEvent.VK_9 => Symbol.Glyph(digit.toChar)
      case KeyEvent.VK_SPACE                                         => Symbol.Glyph(' ')
      case KeyEvent.VK_ENTER                                         => Symbol.Named(InputKey.Enter)
      case KeyEvent.VK_TAB                                           => Symbol.Named(InputKey.Tab)
      case KeyEvent.VK_BACK_SPACE                                    => Symbol.Named(InputKey.Backspace)
      case _                                                         => Symbol.Other
