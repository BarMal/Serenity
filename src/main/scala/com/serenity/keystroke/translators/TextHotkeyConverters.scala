package com.serenity.keystroke.translators

import com.serenity.config.{AppConfig, HotkeyAction}
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*

object TextHotkeyConverters:

  private val actionEvents: List[(HotkeyAction, TextEntryEvent)] = List(
    HotkeyAction.Save                     -> Save,
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
    HotkeyAction.NewTab                   -> NewTab,
    HotkeyAction.CloseTab                 -> CloseTab,
    HotkeyAction.FileSearch               -> FileSearch,
    HotkeyAction.PreviousTab              -> PreviousTab,
    HotkeyAction.NextTab                  -> NextTab
  )

  def hotkeyConverter(config: AppConfig = AppConfig.default): PartialFunction[KeyStrokeInfo, TextEntryEvent] =
    new PartialFunction[KeyStrokeInfo, TextEntryEvent]:
      private val bindings =
        actionEvents.flatMap((action, event) => config.hotkeyConfig.bindingsFor(action).map(_ -> event))

      override def isDefinedAt(info: KeyStrokeInfo): Boolean =
        bindings.exists { case (trigger, _) => trigger.matches(info) }

      override def apply(info: KeyStrokeInfo): TextEntryEvent =
        bindings.collectFirst { case (trigger, event) if trigger.matches(info) => event }.get
