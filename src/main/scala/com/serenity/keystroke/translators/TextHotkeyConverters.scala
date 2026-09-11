package com.serenity.keystroke.translators

import com.serenity.config.{AppConfig, HotkeyAction, HotkeyConfig, HotkeyTrigger}
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*

object TextHotkeyConverters:

  private val actionEvents: List[(HotkeyAction, Event)] = List(
    HotkeyAction.Save                     -> SaveFile,
    HotkeyAction.Quit                     -> Quit,
    HotkeyAction.Undo                     -> Undo,
    HotkeyAction.Redo                     -> Redo,
    HotkeyAction.Copy                     -> Copy,
    HotkeyAction.Paste                    -> Paste,
    HotkeyAction.Cut                      -> Cut,
    HotkeyAction.SelectAll                -> SelectAll,
    HotkeyAction.ToggleSyntaxHighlighting -> ToggleSyntaxHighlighting,
    HotkeyAction.OpenFile                 -> OpenFile,
    HotkeyAction.ToggleCommandRunner      -> ToggleCommandRunner,
    HotkeyAction.ToggleContextualToolbar  -> ToggleContextualToolbar,
    HotkeyAction.NewTab                   -> NewTab,
    HotkeyAction.CloseTab                 -> CloseTab,
    HotkeyAction.SplitPaneHorizontal      -> SplitPaneHorizontal,
    HotkeyAction.SplitPaneVertical        -> SplitPaneVertical,
    HotkeyAction.ClosePane                -> ClosePane,
    HotkeyAction.FileSearch               -> FileSearch,
    HotkeyAction.PreviousTab              -> PreviousTab,
    HotkeyAction.NextTab                  -> NextTab,
    HotkeyAction.Find                     -> OpenFind,
    HotkeyAction.Replace                  -> OpenReplace,
    HotkeyAction.GoToLine                 -> OpenGotoLine,
    HotkeyAction.SaveAs                   -> SaveAsFile,
    HotkeyAction.ToggleShortcutsHelp      -> ToggleShortcutsHelp
  )

  def hotkeyConverter(config: AppConfig = AppConfig.default): PartialFunction[KeyStrokeInfo, Event] =
    HotkeyConfig
      .validate(config.inputConfig.hotkeyConfig.bindings)
      .fold(
        _ => PartialFunction.empty[KeyStrokeInfo, Event],
        _ =>
          val bindings =
            actionEvents.flatMap((action, event) => config.inputConfig.hotkeyConfig.bindingsFor(action).map(_ -> event))

          // `HotkeyTrigger` is an exact-match key (see `HotkeyTrigger.matches`), so a `Map` gives O(1) dispatch in
          // place of the O(n) `collectFirst` scan this used to do per keystroke (issue #1465). Built with `foldLeft`
          // rather than a plain `.toMap` so that, on the rare conflicting binding, the first one encountered in
          // `bindings` wins -- matching `collectFirst`'s original first-match-wins behavior -- instead of `.toMap`'s
          // last-write-wins.
          val lookup = bindings.foldLeft(Map.empty[HotkeyTrigger, Event]) { case (acc, (trigger, event)) =>
            if acc.contains(trigger) then acc else acc + (trigger -> event)
          }

          Function.unlift(lookup.get)
      )
