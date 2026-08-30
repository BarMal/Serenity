package com.serenity.state.reducers

import com.serenity.command.*
import com.serenity.config.HotkeyTrigger
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing

object CommandRunnerReducer:
  private val DoubleTapWindowMillis = 200L

  def reducer(registry: CommandRegistry): Reducer[CommandRunnerEvent] =
    Reducer.instance((event, state) => reduce(event, state, registry))

  def reduce(event: Event, state: AppState, registry: CommandRegistry): ReducerResult =
    CommandRunnerEvent
      .fromEvent(event)
      .map(reduce(_, state, registry))
      .getOrElse(ReducerResult.noEffects(state))

  def reduce(event: CommandRunnerEvent, state: AppState, registry: CommandRegistry): ReducerResult =
    if currentRunner(state).exists(_.isActive) then reduceActive(event, state, registry)
    else ReducerResult.noEffects(state)

  /** Opens the command runner (activating it first if it is closed) directly into its settings view. Moved out of the
    * `OpenSettings` command interpreter, which previously reached into `uiSurfaces` directly — this reducer is where
    * that state is otherwise owned.
    */
  def openSettings(state: AppState, registry: CommandRegistry)(using com.serenity.rope.Balance): AppState =
    val opened = AppEventReducer.reduce(com.serenity.keystroke.events.ToggleCommandRunner, state, registry).state
    opened.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            opened.copy(runtime = opened.runtime.copy(uiSurfaces = opened.runtime.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content = SurfaceContent.CommandPalette(runner.openSettings))
              case current => current
            }))
          case _ => opened
      case None => opened

  private def reduceActive(event: CommandRunnerEvent, state: AppState, registry: CommandRegistry): ReducerResult =
    event match
      case RunnerDismiss =>
        if submenuRecording(state) then ReducerResult.noEffects(clearSubmenuRecording(state))
        else if submenuEditing(state) then ReducerResult.noEffects(clearSubmenuEditMode(state))
        else if submenuSearching(state) then ReducerResult.noEffects(replaceRunner(state, _.updateSubmenuSearch("")))
        else if submenuHasFocus(state) then
          // Escape always means "up one level, or close if there is no level left" (issue #1059) --
          // `SettingsSurfaceState.escape`'s two outcomes are exactly what `exitSubmenuToPreview` already computes via
          // its own `activeSettingsSurface.flatMap(_.pop)`: `Popped` pops with a re-pointed parent, `CloseSurface`
          // clears to the root (not the whole runner -- "closing the settings surface" per `SettingsSurfaceState`'s
          // own doc, not closing the overlay). One call covers both outcomes for both the settings-tab and
          // dedicated-Settings entry points alike, with no more branching on `isSettingsSurface` -- that branch used
          // to preempt this one and always fully deactivate regardless of depth, which was the bug.
          ReducerResult.noEffects(replaceRunner(state, _.exitSubmenuToPreview))
        else if rootEditing(state) then ReducerResult.noEffects(clearRootEditMode(state))
        else ReducerResult.noEffects(deactivate(state))

      case RunnerSubmit =>
        if submenuHasFocus(state) then submitSubmenu(state)
        else if currentRunner(state).exists(_.selectedItem.exists(entersGroupOnSubmit))
        then ReducerResult.noEffects(replaceRunner(state, _.enterSelectedGroup))
        else
          currentRunner(state) match
            case None =>
              // Unreachable: `reduce` only dispatches here when currentRunner(state) is Some and
              // active. Mirrors the fallback below for the no-selection case.
              ReducerResult.noEffects(deactivate(state))
            case Some(runner) =>
              runner.editingItemId match
                case Some(itemId) =>
                  runner.inputItems.find(_.id == itemId) match
                    case Some(item) =>
                      item.parse(runner.editingText) match
                        case Some(intent) =>
                          val cmd = Command.typed(itemId, item.label, intent, CommandCategory.Settings)
                          ReducerResult(
                            state = replaceRunner(
                              state,
                              r => r.copy(editingItemId = None, editingText = "", statusMessage = None)
                            ),
                            effects = List(AppEffect.ExecuteCommand(cmd))
                          )
                        case None =>
                          ReducerResult.noEffects(
                            replaceRunner(
                              state,
                              _.copy(statusMessage = Some(invalidInputMessage(item, runner.editingText)))
                            )
                          )
                    case None =>
                      ReducerResult.noEffects(state)

                case None =>
                  runner.selectedItem match
                    case Some(_: CommandSurfaceItem.InputItem) =>
                      ReducerResult.noEffects(state)
                    case Some(CommandSurfaceItem.CommandItem(command))
                        if command.intent == CommandIntent.Settings(
                          SettingsIntent.General(GeneralSettingsIntent.OpenSettings)
                        ) =>
                      ReducerResult.noEffects(replaceRunner(state, _.openSettings))
                    case Some(CommandSurfaceItem.CommandItem(command)) =>
                      ReducerResult(
                        state = deactivate(state),
                        effects = List(AppEffect.ExecuteCommand(command))
                      )
                    case Some(option: CommandSurfaceItem.OptionItem) =>
                      option.selectedIntent match
                        case Some(intent) =>
                          ReducerResult(
                            state = state,
                            effects = List(
                              AppEffect.ExecuteCommand(Command.typed(option.id, option.label, intent, option.category))
                            )
                          )
                        case None =>
                          ReducerResult.noEffects(state)
                    case _ =>
                      ReducerResult.noEffects(deactivate(state))

      case RunnerInsertChar(char) =>
        if submenuHasFocus(state) then
          currentRunner(state) match
            case Some(runner) =>
              runner.activeSettingsSurface match
                case Some(surface) =>
                  val page          = surface.current
                  val allItems      = runner.submenuItems(page.groupId)
                  val editingItemId = page.editingItemId
                  val editingText   = page.draftText
                  val selectedInput = runner.focusedSubmenuItems.lift(runner.settingsSurfaceSelectedIndex).collect {
                    case input: CommandSurfaceItem.InputItem => input
                  }
                  val activeInput = allItems
                    .collectFirst {
                      case input: CommandSurfaceItem.InputItem if editingItemId.contains(input.id) => input
                    }
                    .orElse(selectedInput)
                  if activeInput.exists(_.accepts(editingText, char)) then
                    activeInput match
                      case Some(item) =>
                        val nextText = if editingItemId.contains(item.id) then editingText + char else char.toString
                        ReducerResult.noEffects(
                          replaceRunner(
                            state,
                            r => r.withSubmenuEditingItem(item.id, nextText).copy(statusMessage = None)
                          )
                        )
                      case None =>
                        ReducerResult.noEffects(replaceRunner(state, _.updateSubmenuSearch(page.searchTerm + char)))
                  else if editingItemId.isEmpty then
                    ReducerResult.noEffects(replaceRunner(state, _.updateSubmenuSearch(page.searchTerm + char)))
                  else ReducerResult.noEffects(state)
                case None =>
                  ReducerResult.noEffects(state)
            case None =>
              ReducerResult.noEffects(state)
        else
          currentRunner(state) match
            case Some(runner) =>
              runner.editingItemId match
                case Some(itemId) =>
                  val item = runner.inputItems.find(_.id == itemId)
                  if item.exists(_.accepts(runner.editingText, char)) then
                    ReducerResult.noEffects(
                      replaceRunner(state, r => r.copy(editingText = r.editingText + char, statusMessage = None))
                    )
                  else ReducerResult.noEffects(state)
                case None =>
                  runner.selectedItem match
                    case Some(item: CommandSurfaceItem.InputItem) =>
                      if item.accepts("", char) then
                        ReducerResult.noEffects(
                          replaceRunner(
                            state,
                            r =>
                              r.copy(editingItemId = Some(item.id), editingText = char.toString, statusMessage = None)
                          )
                        )
                      else ReducerResult.noEffects(state)
                    case _ =>
                      given CommandRegistry = registry
                      ReducerResult.noEffects(
                        replaceRunner(state, r => r.updateSearchTerm(r.searchTerm + char))
                      )
            case None =>
              ReducerResult.noEffects(state)

      case RunnerRecordBinding(info, recordedAtMillis) =>
        recordBinding(state, info, recordedAtMillis)

      case RunnerBindingRecordingExpired(recordedAtMillis) =>
        expireRecordedBinding(state, recordedAtMillis)

      case RunnerDeleteBackward =>
        if submenuHasFocus(state) then
          // Backspace always means "delete one character of the current page's text", via
          // `SettingsSurfaceState.deleteBackward` -- never a navigation, and a no-op when there is no text to delete
          // (issue #1059's fix: this used to fall through to `exitSubmenuToPreview`, silently navigating up a level
          // on an empty-text Backspace).
          ReducerResult.noEffects(replaceRunner(state, _.deleteSubmenuTextBackward))
        else
          currentRunner(state).flatMap(_.editingItemId) match
            case Some(_) =>
              if currentRunner(state).exists(_.editingText.nonEmpty) then
                ReducerResult.noEffects(
                  replaceRunner(state, r => r.copy(editingText = r.editingText.dropRight(1), statusMessage = None))
                )
              else ReducerResult.noEffects(state)
            case None =>
              if currentRunner(state).exists(_.searchTerm.nonEmpty) then
                given CommandRegistry = registry
                ReducerResult.noEffects(
                  replaceRunner(state, runner => runner.updateSearchTerm(runner.searchTerm.dropRight(1)))
                )
              else ReducerResult.noEffects(state)

      case RunnerDeleteForward =>
        if submenuHasFocus(state) then
          currentRunner(state).flatMap(_.activeSettingsSurface) match
            case Some(surface) if surface.current.editingItemId.nonEmpty =>
              ReducerResult.noEffects(state)
            case _ =>
              ReducerResult.noEffects(state)
        else
          currentRunner(state).flatMap(_.editingItemId) match
            case Some(_) =>
              ReducerResult.noEffects(state)
            case None =>
              ReducerResult.noEffects(state)

      case RunnerDeleteWordBackward =>
        if submenuHasFocus(state) then
          currentRunner(state).flatMap(_.activeSettingsSurface) match
            case Some(surface) if surface.current.editingItemId.nonEmpty && surface.current.draftText.nonEmpty =>
              ReducerResult.noEffects(
                replaceRunner(
                  state,
                  r =>
                    r.withSubmenuEditingText(TextEditing.deleteWordBackward(surface.current.draftText))
                      .copy(statusMessage = None)
                )
              )
            case Some(surface) if surface.current.searchTerm.nonEmpty =>
              ReducerResult.noEffects(
                replaceRunner(state, _.updateSubmenuSearch(TextEditing.deleteWordBackward(surface.current.searchTerm)))
              )
            case _ =>
              ReducerResult.noEffects(state)
        else
          currentRunner(state).flatMap(_.editingItemId) match
            case Some(_) =>
              if currentRunner(state).exists(_.editingText.nonEmpty) then
                ReducerResult.noEffects(
                  replaceRunner(
                    state,
                    r => r.copy(editingText = TextEditing.deleteWordBackward(r.editingText), statusMessage = None)
                  )
                )
              else ReducerResult.noEffects(state)
            case None =>
              if currentRunner(state).exists(_.searchTerm.nonEmpty) then
                given CommandRegistry = registry
                ReducerResult.noEffects(
                  replaceRunner(
                    state,
                    runner => runner.updateSearchTerm(TextEditing.deleteWordBackward(runner.searchTerm))
                  )
                )
              else ReducerResult.noEffects(state)

      case RunnerDeleteWordForward =>
        if submenuHasFocus(state) then
          currentRunner(state).flatMap(_.activeSettingsSurface) match
            case Some(surface) if surface.current.editingItemId.nonEmpty && surface.current.draftText.nonEmpty =>
              ReducerResult.noEffects(
                replaceRunner(
                  state,
                  r =>
                    r.withSubmenuEditingText(TextEditing.deleteWordForward(surface.current.draftText))
                      .copy(statusMessage = None)
                )
              )
            case Some(surface) if surface.current.searchTerm.nonEmpty =>
              ReducerResult.noEffects(
                replaceRunner(state, _.updateSubmenuSearch(TextEditing.deleteWordForward(surface.current.searchTerm)))
              )
            case _ =>
              ReducerResult.noEffects(state)
        else
          currentRunner(state).flatMap(_.editingItemId) match
            case Some(_) =>
              if currentRunner(state).exists(_.editingText.nonEmpty) then
                ReducerResult.noEffects(
                  replaceRunner(
                    state,
                    r => r.copy(editingText = TextEditing.deleteWordForward(r.editingText), statusMessage = None)
                  )
                )
              else ReducerResult.noEffects(state)
            case None =>
              if currentRunner(state).exists(_.searchTerm.nonEmpty) then
                given CommandRegistry = registry
                ReducerResult.noEffects(
                  replaceRunner(
                    state,
                    runner => runner.updateSearchTerm(TextEditing.deleteWordForward(runner.searchTerm))
                  )
                )
              else ReducerResult.noEffects(state)

      case RunnerPaste =>
        state.runtime.clipboard
          .getOrElse("")
          .filter(char => char != '\r' && char != '\n')
          .foldLeft(ReducerResult.noEffects(state))((result, char) =>
            reduceActive(RunnerInsertChar(char), result.state, registry)
          )

      case RunnerNavigate(Direction.Up) =>
        if submenuHasFocus(state) then ReducerResult.noEffects(replaceRunner(state, _.moveSubmenuSelection(-1)))
        else ReducerResult.noEffects(replaceRunner(state, _.moveSelection(-1)))

      case RunnerNavigate(Direction.Down) =>
        if submenuHasFocus(state) then ReducerResult.noEffects(replaceRunner(state, _.moveSubmenuSelection(1)))
        else ReducerResult.noEffects(replaceRunner(state, _.moveSelection(1)))

      case RunnerSelectVisibleItem(index) =>
        ReducerResult.noEffects(replaceRunner(state, _.withSelectedVisibleIndex(index)))

      case RunnerSelectSubmenuItem(index) =>
        ReducerResult.noEffects(replaceRunner(state, _.withSelectedFocusedSubmenuIndex(index)))

      case RunnerNavigate(Direction.Left) =>
        if submenuHasFocus(state) then
          currentRunner(state) match
            case Some(runner) =>
              val updatedRunner = runner.adjustSelectedSubmenuOption(-1)
              val effects = submenuSelectedOption(updatedRunner)
                .flatMap(_.selectedIntent)
                .toList
                .map(intent =>
                  AppEffect.ExecuteCommand(
                    Command.typed(intent.toString, intent.toString, intent, CommandCategory.Settings)
                  )
                )
              ReducerResult(replaceRunner(state, _ => updatedRunner), effects)
            case None => ReducerResult.noEffects(state)
        else
          currentRunner(state) match
            case Some(runner) if runner.searchTerm.isEmpty && runner.editingItemId.isEmpty =>
              runner.selectedItem match
                case Some(option: CommandSurfaceItem.OptionItem) =>
                  val nextIndex     = (option.selectedIndex - 1 + option.options.length) % option.options.length
                  val nextOption    = option.options(nextIndex)
                  val updatedRunner = runner.adjustSelectedOption(-1)
                  val effects =
                    List(
                      AppEffect.ExecuteCommand(
                        Command.typed(option.id, option.label, nextOption.intent, option.category)
                      )
                    )
                  ReducerResult(replaceRunner(state, _ => updatedRunner), effects)
                case _ =>
                  ReducerResult.noEffects(state)
            case _ =>
              ReducerResult.noEffects(state)

      case RunnerNavigate(Direction.Right) =>
        if submenuHasFocus(state) then
          currentRunner(state) match
            case Some(runner) =>
              val updatedRunner = runner.adjustSelectedSubmenuOption(1)
              val effects = submenuSelectedOption(updatedRunner)
                .flatMap(_.selectedIntent)
                .toList
                .map(intent =>
                  AppEffect.ExecuteCommand(
                    Command.typed(intent.toString, intent.toString, intent, CommandCategory.Settings)
                  )
                )
              ReducerResult(replaceRunner(state, _ => updatedRunner), effects)
            case None => ReducerResult.noEffects(state)
        else
          currentRunner(state) match
            case Some(runner) if runner.searchTerm.isEmpty && runner.editingItemId.isEmpty =>
              runner.selectedItem match
                case Some(option: CommandSurfaceItem.OptionItem) =>
                  val nextIndex     = (option.selectedIndex + 1) % option.options.length
                  val nextIntent    = option.options(nextIndex).intent
                  val updatedRunner = runner.adjustSelectedOption(1)
                  val effects =
                    List(AppEffect.ExecuteCommand(Command.typed(option.id, option.label, nextIntent, option.category)))
                  ReducerResult(replaceRunner(state, _ => updatedRunner), effects)
                case _ =>
                  ReducerResult.noEffects(state)
            case _ =>
              ReducerResult.noEffects(state)

  private def deactivate(state: AppState): AppState =
    state
      .copy(runtime =
        state.runtime.copy(uiSurfaces =
          state.runtime.uiSurfaces.filterNot(surface => state.commandRunnerSurface.exists(_.id == surface.id))
        )
      )
      .popFocus

  private def currentRunner(state: AppState): Option[CommandRunner] =
    state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }

  /** The drilled-in settings page, if `state`'s runner is on one -- `None` both when there's no settings surface at all
    * (`CommandRunnerSurface.Palette`) and when there is one but nothing has been entered yet
    * (`CommandRunnerSurface.Settings(_, None)`, the settings root). Dispatch through `CommandRunnerSurface` (issue
    * #931, Stage 2) rather than reading `activeSettingsSurface` directly -- equivalent by construction (`surface`
    * carries `activeSettingsSurface` as its `Settings` payload verbatim), but names the type this stage introduces as
    * the seam these submenu-focus checks are really keyed on.
    */
  private def activeSubmenu(state: AppState): Option[SettingsSurfaceState] =
    currentRunner(state).flatMap(_.surface match
      case CommandRunnerSurface.Settings(_, drilled) => drilled
      case CommandRunnerSurface.Palette(_)           => None)

  /** Items that open a nested surface on submit rather than executing an action. */
  private def entersGroupOnSubmit(item: CommandSurfaceItem): Boolean =
    item match
      case _: CommandSurfaceItem.GroupItem | _: CommandSurfaceItem.SettingSearchItem => true
      case _                                                                         => false

  /** Both entry points now render a drilled-in settings group on the one `CommandPalette` surface (issue #1059), so a
    * drilled-in page being present is the whole signal -- there is no second surface to focus, and `isSettingsSurface`
    * no longer needs distinguishing here since the two paths behave identically once inside a group.
    */
  private def submenuHasFocus(state: AppState): Boolean =
    activeSubmenu(state).nonEmpty

  private def submenuEditing(state: AppState): Boolean =
    activeSubmenu(state).exists(_.current.editingItemId.nonEmpty)

  private def submenuSearching(state: AppState): Boolean =
    activeSubmenu(state).exists(_.current.searchTerm.nonEmpty)

  private def rootEditing(state: AppState): Boolean =
    currentRunner(state).flatMap(_.editingItemId).nonEmpty

  private def clearSubmenuEditMode(state: AppState): AppState =
    replaceRunner(state, runner => runner.cancelSubmenuEditingText.copy(statusMessage = None))

  private def clearRootEditMode(state: AppState): AppState =
    replaceRunner(state, _.copy(editingItemId = None, editingText = "", statusMessage = None))

  private def submenuSelectedOption(runner: CommandRunner): Option[CommandSurfaceItem.OptionItem] =
    runner.activeSettingsSurface.flatMap { _ =>
      runner.focusedSubmenuItems.lift(runner.settingsSurfaceSelectedIndex).collect {
        case option: CommandSurfaceItem.OptionItem => option
      }
    }

  private def submitSubmenu(state: AppState): ReducerResult =
    currentRunner(state) match
      case None =>
        ReducerResult.noEffects(state)
      case Some(runner) =>
        runner.activeSettingsSurface match
          case None =>
            ReducerResult.noEffects(state)
          case Some(surface) =>
            val page = surface.current
            runner.focusedSubmenuItems.lift(runner.settingsSurfaceSelectedIndex) match
              case Some(item: CommandSurfaceItem.InputItem) if page.editingItemId.isEmpty && item.acceptsBindingText =>
                ReducerResult.noEffects(
                  replaceRunner(
                    state,
                    r =>
                      r.beginSubmenuRecording(item.id).copy(statusMessage = Some("Press a key or shortcut to assign"))
                  )
                )
              case Some(_: CommandSurfaceItem.InputItem) if page.editingItemId.isEmpty =>
                ReducerResult.noEffects(state)
              case Some(item: CommandSurfaceItem.InputItem)
                  if page.recording.flatMap(_.pendingGlobalHotkeyConflict).nonEmpty =>
                page.recording.flatMap(_.pendingGlobalHotkeyConflict).fold(ReducerResult.noEffects(state)) {
                  case (action, binding) =>
                    ReducerResult(
                      state = replaceRunner(
                        state,
                        r => r.clearSubmenuEditingAndRecording.copy(statusMessage = None)
                      ),
                      effects = List(
                        AppEffect.ExecuteCommand(
                          Command.typed(
                            item.id,
                            item.label,
                            CommandIntent.Keybindings(KeybindingsIntent.ResolveGlobalHotkeyConflict(action, binding)),
                            item.category
                          )
                        )
                      )
                    )
                }
              case Some(item: CommandSurfaceItem.InputItem)
                  if page.recording.flatMap(_.pendingFocusedKeymapConflict).nonEmpty =>
                page.recording.flatMap(_.pendingFocusedKeymapConflict).fold(ReducerResult.noEffects(state)) {
                  case (itemId, binding) =>
                    ReducerResult(
                      state = replaceRunner(
                        state,
                        r => r.clearSubmenuEditingAndRecording.copy(statusMessage = None)
                      ),
                      effects = List(
                        AppEffect.ExecuteCommand(
                          Command.typed(
                            item.id,
                            item.label,
                            CommandIntent.Keybindings(KeybindingsIntent.ResolveFocusedKeymapConflict(itemId, binding)),
                            item.category
                          )
                        )
                      )
                    )
                }
              case Some(item: CommandSurfaceItem.InputItem) =>
                item.parse(page.draftText) match
                  case Some(intent) =>
                    ReducerResult(
                      state = replaceRunner(
                        state,
                        r => r.clearSubmenuEditingAndRecording.copy(statusMessage = None)
                      ),
                      effects =
                        List(AppEffect.ExecuteCommand(Command.typed(item.id, item.label, intent, item.category)))
                    )
                  case None =>
                    ReducerResult.noEffects(
                      replaceRunner(
                        state,
                        _.copy(statusMessage = Some(invalidInputMessage(item, page.draftText)))
                      )
                    )
              case Some(option: CommandSurfaceItem.OptionItem) =>
                option.selectedIntent match
                  case Some(intent) =>
                    ReducerResult(
                      state = state,
                      effects =
                        List(AppEffect.ExecuteCommand(Command.typed(option.id, option.label, intent, option.category)))
                    )
                  case None =>
                    ReducerResult.noEffects(state)
              case Some(CommandSurfaceItem.CommandItem(command)) =>
                ReducerResult(
                  state = deactivate(state),
                  effects = List(AppEffect.ExecuteCommand(command))
                )
              case Some(_: CommandSurfaceItem.GroupItem) =>
                ReducerResult.noEffects(replaceRunner(state, _.enterSelectedSubmenuGroup))
              case _ =>
                ReducerResult.noEffects(state)

  private def invalidInputMessage(item: CommandSurfaceItem.InputItem, text: String): String =
    val value = if text.trim.isEmpty then "<empty>" else text
    if item.acceptsBindingText then s"Invalid binding: $value"
    else s"Invalid value: $value"

  private def recordBinding(
    state: AppState,
    info: com.serenity.keystroke.KeyStrokeInfo,
    recordedAtMillis: Long
  ): ReducerResult =
    currentRunner(state) match
      case None =>
        ReducerResult.noEffects(state)
      case Some(runner) =>
        runner.activeSettingsSurface match
          case None =>
            ReducerResult.noEffects(state)
          case Some(surface) =>
            surface.current.recording match
              case None =>
                ReducerResult.noEffects(state)
              case Some(recording) =>
                runner.submenuItems(surface.current.groupId).find(_.id == recording.itemId) match
                  case Some(item: CommandSurfaceItem.InputItem) =>
                    recording.pendingRecordedBinding match
                      case None =>
                        ReducerResult(
                          replaceRunner(
                            state,
                            current =>
                              current
                                .withPendingRecordedBinding(info, recordedAtMillis)
                                .copy(statusMessage =
                                  Some("Press the same key again within 200ms to record a double tap")
                                )
                          ),
                          List(AppEffect.ScheduleCommandRunnerBindingExpiry(recordedAtMillis))
                        )
                      case Some((first, firstAt))
                          if recordedAtMillis >= firstAt &&
                            recordedAtMillis - firstAt <= DoubleTapWindowMillis &&
                            sameKeyStroke(first, info) =>
                        assignRecordedBinding(state, item, first)
                      case Some((first, _)) =>
                        assignRecordedBinding(state, item, first)
                  case _ => ReducerResult.noEffects(state)

  private def expireRecordedBinding(state: AppState, recordedAtMillis: Long): ReducerResult =
    currentRunner(state)
      .flatMap { runner =>
        runner.activeSettingsSurface.flatMap { surface =>
          surface.current.recording.flatMap { recording =>
            recording.pendingRecordedBinding match
              case Some((first, pendingAt)) if pendingAt == recordedAtMillis =>
                runner.submenuItems(surface.current.groupId).find(_.id == recording.itemId) match
                  case Some(item: CommandSurfaceItem.InputItem) => Some(assignRecordedBinding(state, item, first))
                  case _                                        => None
              case _ => None
          }
        }
      }
      .getOrElse(ReducerResult.noEffects(state))

  private def sameKeyStroke(
    left: com.serenity.keystroke.KeyStrokeInfo,
    right: com.serenity.keystroke.KeyStrokeInfo
  ): Boolean =
    left.keyType == right.keyType && left.character == right.character && left.modifiers == right.modifiers

  private def assignRecordedBinding(
    state: AppState,
    item: CommandSurfaceItem.InputItem,
    first: com.serenity.keystroke.KeyStrokeInfo
  ): ReducerResult =
    val trigger = HotkeyTrigger(first.keyType, first.character, first.modifiers)
    val binding = trigger.render
    item.parse(binding) match
      case Some(intent) =>
        ReducerResult(
          state = replaceRunner(
            state,
            current =>
              current.clearSubmenuEditingAndRecording
                .copy(statusMessage = bareModifierFidelityWarning(current, trigger, binding))
          ),
          effects = List(AppEffect.ExecuteCommand(Command.typed(item.id, item.label, intent, item.category)))
        )
      case None =>
        ReducerResult.noEffects(
          replaceRunner(state, _.copy(statusMessage = Some(invalidInputMessage(item, binding))))
        )

  /** Issue #1194: a bare-modifier double tap (`ctrl+ctrl`, ...) has no representation in xterm's `modifyOtherKeys` wire
    * format -- there is no bare press/release event for a lone modifier in that protocol, only in the kitty keyboard
    * protocol's flags -- so recording one on a TUI session capped at [[KeyboardFidelityTier.ModifyOtherKeys]] would
    * otherwise silently record a binding that can never fire. GUI mode and a kitty-tier TUI session are always
    * [[KeyboardFidelityTier.Full]], so this never fires there.
    */
  private def bareModifierFidelityWarning(
    runner: CommandRunner,
    trigger: HotkeyTrigger,
    binding: String
  ): Option[String] =
    Option.when(
      runner.isTuiMode &&
        runner.keyboardFidelityTier == KeyboardFidelityTier.ModifyOtherKeys &&
        trigger.isBareModifierChord
    )(
      s"\"$binding\" recorded, but won't fire -- this terminal can't send a bare-modifier key event " +
        "at its negotiated keyboard protocol tier"
    )

  private def submenuRecording(state: AppState): Boolean =
    activeSubmenu(state).exists(_.current.recording.nonEmpty)

  private def clearSubmenuRecording(state: AppState): AppState =
    replaceRunner(state, runner => runner.clearSubmenuEditingAndRecording.copy(statusMessage = None))

  /** Settings navigation -- both the settings-tab-in-palette and the dedicated Settings surface -- renders entirely
    * through `SurfaceContentResolver.resolveSettingsSurface` on the one `CommandPalette` surface (issue #1059's "one
    * consistent settings experience"): capped group-preview rows expand in place in that single list instead of a
    * second floating surface, so every runner update just rebuilds the one surface and reasserts focus on it.
    */
  private def replaceRunner(state: AppState, update: CommandRunner => CommandRunner): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val updatedRunner = update(runner)
            val updatedSurfaces = state.runtime.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              case other =>
                other
            }
            state.copy(
              runtime = state.runtime.copy(uiSurfaces = updatedSurfaces),
              persisted = state.persisted.copy(focus = Focus.Surface(surface.id))
            )
          case _ =>
            state
      case None =>
        state
