package com.serenity.keystroke.translators

import com.serenity.config.{AppConfig, HotkeyAction, HotkeyConfig}
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

          Function.unlift(info => bindings.collectFirst { case (trigger, event) if trigger.matches(info) => event })
      )
