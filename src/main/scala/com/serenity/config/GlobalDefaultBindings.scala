package com.serenity.config

import com.serenity.keystroke.{InputKey, Modifier}

/** The default global bindings that sit outside the primary-modifier table in [[HotkeyConfig.defaultBindingsFor]]:
  * directional focus, Go to File and the chapter-note keys.
  */
private[config] object GlobalDefaultBindings:

  // Plain Alt rather than the primary modifier, so it is the same on every platform with no terminal rewrite. Nothing
  // else binds Alt+Arrow, and the editor's word moves stay on Ctrl+Arrow.
  def directionalFocus: Map[HotkeyAction, List[HotkeyTrigger]] =
    Map(
      HotkeyAction.FocusLeft  -> InputKey.ArrowLeft,
      HotkeyAction.FocusRight -> InputKey.ArrowRight,
      HotkeyAction.FocusUp    -> InputKey.ArrowUp,
      HotkeyAction.FocusDown  -> InputKey.ArrowDown
    ).view.mapValues(key => List(HotkeyTrigger(key, None, Set(Modifier.Alt)))).toMap

  // Ctrl+G is Go to Line, so the chapter-note keys live on the shifted letters. None of them is Ctrl+Shift+H/I/J/M,
  // which collapse into Backspace/Tab/Enter on terminals that cannot tell Ctrl+Shift+letter from Ctrl+letter.
  // Ctrl+Shift+L is left to Select All Occurrences, as in VS Code and Sublime Text, so the notes pin sits on Alt.
  def chapterNotes(primaryModifier: Modifier): Map[HotkeyAction, List[HotkeyTrigger]] =
    def chord(key: Char, extra: Modifier): List[HotkeyTrigger] =
      List(HotkeyTrigger(InputKey.Character, Some(key), Set(primaryModifier, extra)))
    Map(
      HotkeyAction.ToggleChapterGhosts -> chord('g', Modifier.Shift),
      HotkeyAction.OpenChapterNote     -> chord('n', Modifier.Shift),
      HotkeyAction.OpenKeywordNote     -> chord('k', Modifier.Shift),
      HotkeyAction.ToggleNotesPin      -> chord('p', Modifier.Alt)
    )

  // VS Code's alternative Quick Open key: its main one, the primary modifier with P, is the command runner here.
  def goToFile(primaryModifier: Modifier): Map[HotkeyAction, List[HotkeyTrigger]] =
    Map(HotkeyAction.GoToFile -> List(HotkeyTrigger(InputKey.Character, Some('e'), Set(primaryModifier))))
