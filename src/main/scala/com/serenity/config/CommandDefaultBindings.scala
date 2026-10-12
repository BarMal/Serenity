package com.serenity.config

import com.serenity.keystroke.{InputKey, Modifier}

/** The keys shipped for registry commands that have no [[HotkeyAction]]: the prose formatting toggles (issue #1858),
  * the comment toggles (issue #1997) and the occurrence commands (issue #1994). A terminal sends Ctrl+I as Tab, and
  * `TerminalInputDecoder` keeps it Tab, so italic's key reaches only the GUI and terminals that report modified keys
  * distinctly -- the Tab key itself is never taken.
  */
private[config] object CommandDefaultBindings:

  def forPrimaryModifier(primaryModifier: Modifier): Map[String, List[HotkeyTrigger]] =
    formatting(primaryModifier) ++ comments(primaryModifier) ++ occurrences(primaryModifier)

  private def formatting(primaryModifier: Modifier): Map[String, List[HotkeyTrigger]] =
    Map("bold" -> 'b', "italic" -> 'i', "underline" -> 'u').view
      .mapValues(key => List(HotkeyTrigger(InputKey.Character, Some(key), Set(primaryModifier))))
      .toMap

  // VS Code's keys. A terminal sends Ctrl+/ as the control byte 0x1F, which `TerminalInputDecoder` reports as Ctrl+_,
  // so that is a second key for the line comment rather than a rewrite of the first.
  private def comments(primaryModifier: Modifier): Map[String, List[HotkeyTrigger]] =
    Map(
      "toggle-line-comment" -> List(
        HotkeyTrigger(InputKey.Character, Some('/'), Set(primaryModifier)),
        HotkeyTrigger(InputKey.Character, Some('_'), Set(Modifier.Ctrl))
      ),
      "toggle-block-comment" -> List(HotkeyTrigger(InputKey.Character, Some('a'), Set(Modifier.Alt, Modifier.Shift)))
    )

  // VS Code's and Sublime Text's Ctrl+D and Ctrl+Shift+L. Skipping is Ctrl+K Ctrl+D there; a binding here is one key,
  // so it takes Alt+K beside Ctrl+D.
  private def occurrences(primaryModifier: Modifier): Map[String, List[HotkeyTrigger]] =
    def chord(key: Char, extra: Modifier*): List[HotkeyTrigger] =
      List(HotkeyTrigger(InputKey.Character, Some(key), Set(primaryModifier) ++ extra))
    Map(
      "select-next-occurrence" -> chord('d'),
      "skip-occurrence"        -> chord('k', Modifier.Alt),
      "select-all-occurrences" -> chord('l', Modifier.Shift)
    )
