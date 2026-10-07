package com.serenity.keystroke.translators

import com.serenity.config.{AppConfig, HotkeyAction, HotkeyConfig, HotkeyTrigger}
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*

object TextHotkeyConverters:

  private[serenity] val actionEvents: List[(HotkeyAction, Event)] = List(
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
    HotkeyAction.GoToFile                 -> GoToFile,
    HotkeyAction.PreviousTab              -> PreviousTab,
    HotkeyAction.NextTab                  -> NextTab,
    HotkeyAction.MoveTabLeft              -> MoveTabLeft,
    HotkeyAction.MoveTabRight             -> MoveTabRight,
    HotkeyAction.Find                     -> OpenFind,
    HotkeyAction.Replace                  -> OpenReplace,
    HotkeyAction.GoToLine                 -> OpenGotoLine,
    HotkeyAction.SaveAs                   -> SaveAsFile,
    HotkeyAction.ToggleShortcutsHelp      -> ToggleShortcutsHelp,
    HotkeyAction.FocusLeft                -> FocusInDirection(Direction.Left),
    HotkeyAction.FocusRight               -> FocusInDirection(Direction.Right),
    HotkeyAction.FocusUp                  -> FocusInDirection(Direction.Up),
    HotkeyAction.FocusDown                -> FocusInDirection(Direction.Down),
    HotkeyAction.ToggleChapterGhosts      -> ToggleChapterGhosts,
    HotkeyAction.OpenChapterNote          -> OpenChapterNote,
    HotkeyAction.ToggleNotesPin           -> ToggleNotesPin
  )

  def hotkeyConverter(config: AppConfig = AppConfig.default): PartialFunction[KeyStrokeInfo, Event] =
    val hotkeys = config.inputConfig.hotkeyConfig
    HotkeyConfig
      .validate(hotkeys)
      .fold(
        _ => PartialFunction.empty[KeyStrokeInfo, Event],
        _ =>
          val bindings =
            actionEvents.flatMap((action, event) => hotkeys.bindingsFor(action).map(_ -> event)) ++
              hotkeys.commandBindings.toList.flatMap((commandId, triggers) => triggers.map(_ -> RunCommand(commandId)))

          // `HotkeyTrigger` is an exact-match key (see `HotkeyTrigger.matches`), so a `Map` gives O(1) dispatch in
          // place of the O(n) `collectFirst` scan this used to do per keystroke (issue #1465). Built with `foldLeft`
          // rather than a plain `.toMap` so that, on the rare conflicting binding, the first one encountered in
          // `bindings` wins -- matching `collectFirst`'s original first-match-wins behavior -- instead of `.toMap`'s
          // last-write-wins.
          val lookup = bindings.foldLeft(Map.empty[HotkeyTrigger, Event]) {
            case (acc, (trigger, event)) =>
              if acc.contains(trigger) then acc else acc + (trigger -> event)
          }

          Function.unlift(info => lookup.get(HotkeyTrigger(info.keyType, info.character, info.modifiers)))
      )
