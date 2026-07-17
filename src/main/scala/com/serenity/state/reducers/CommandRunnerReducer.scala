package com.serenity.state.reducers

import com.serenity.command.*
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing

object CommandRunnerReducer:
  private val SubmenuSurfaceId = SurfaceId("command-runner-submenu")

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

  private def reduceActive(event: CommandRunnerEvent, state: AppState, registry: CommandRegistry): ReducerResult =
    event match
      case RunnerDismiss =>
        if submenuEditing(state) then ReducerResult.noEffects(clearSubmenuEditMode(state))
        else if submenuSearching(state) then ReducerResult.noEffects(replaceRunner(state, _.updateSubmenuSearch("")))
        else if submenuHasFocus(state) then ReducerResult.noEffects(replaceRunner(state, _.exitSubmenuToPreview))
        else if rootEditing(state) then ReducerResult.noEffects(clearRootEditMode(state))
        else ReducerResult.noEffects(deactivate(state))

      case RunnerSubmit =>
        if submenuHasFocus(state) then submitSubmenu(state)
        else if currentRunner(state).exists(
            _.selectedItem.exists(item =>
              item.isInstanceOf[CommandSurfaceItem.GroupItem] || item.isInstanceOf[CommandSurfaceItem.SettingSearchItem]
            )
          )
        then
          ReducerResult.noEffects(replaceRunner(state, _.enterSelectedGroup))
        else
          currentRunner(state).flatMap(_.editingItemId) match
            case Some(itemId) =>
              val runner = currentRunner(state).get
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
              currentRunner(state).flatMap(_.selectedItem) match
                case Some(_: CommandSurfaceItem.InputItem) =>
                  ReducerResult.noEffects(state)
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
          currentRunner(state).flatMap(_.activeSubmenu) match
            case Some(submenu) =>
              val runner   = currentRunner(state).get
              val allItems = runner.submenuItems(submenu.groupId)
              val selectedInput =
                submenu.selectedItemFromAll(allItems).collect {
                  case input: CommandSurfaceItem.InputItem =>
                    input
                }
              val activeInput = allItems
                .collectFirst {
                  case input: CommandSurfaceItem.InputItem if submenu.editingItemId.contains(input.id) => input
                }
                .orElse(selectedInput)
              val currentText = Option.when(submenu.editingItemId.nonEmpty)(submenu.editingText).getOrElse("")
              if activeInput.exists(_.accepts(currentText, char)) then
                activeInput match
                  case Some(item) =>
                    val nextText =
                      if submenu.editingItemId.contains(item.id) then submenu.editingText + char else char.toString
                    ReducerResult.noEffects(
                      replaceRunner(
                        state,
                        r =>
                          r.copy(
                            activeSubmenu =
                              r.activeSubmenu.map(s => s.copy(editingItemId = Some(item.id), editingText = nextText))
                          ).copy(statusMessage = None)
                      )
                    )
                  case None =>
                    ReducerResult.noEffects(replaceRunner(state, _.updateSubmenuSearch(submenu.searchTerm + char)))
              else if submenu.editingItemId.isEmpty then
                ReducerResult.noEffects(replaceRunner(state, _.updateSubmenuSearch(submenu.searchTerm + char)))
              else ReducerResult.noEffects(state)
            case None =>
              ReducerResult.noEffects(state)
        else
          currentRunner(state).flatMap(_.editingItemId) match
            case Some(itemId) =>
              val runner = currentRunner(state).get
              val item   = runner.inputItems.find(_.id == itemId)
              if item.exists(_.accepts(runner.editingText, char)) then
                ReducerResult.noEffects(
                  replaceRunner(state, r => r.copy(editingText = r.editingText + char, statusMessage = None))
                )
              else ReducerResult.noEffects(state)
            case None =>
              currentRunner(state).flatMap(_.selectedItem) match
                case Some(item: CommandSurfaceItem.InputItem) =>
                  if item.accepts("", char) then
                    ReducerResult.noEffects(
                      replaceRunner(
                        state,
                        runner =>
                          runner.copy(editingItemId = Some(item.id), editingText = char.toString, statusMessage = None)
                      )
                    )
                  else ReducerResult.noEffects(state)
                case _ =>
                  given CommandRegistry = registry
                  ReducerResult.noEffects(
                    replaceRunner(state, runner => runner.updateSearchTerm(runner.searchTerm + char))
                  )

      case RunnerDeleteBackward =>
        if submenuHasFocus(state) then
          currentRunner(state).flatMap(_.activeSubmenu) match
            case Some(submenu) if submenu.editingItemId.nonEmpty && submenu.editingText.nonEmpty =>
              ReducerResult.noEffects(
                replaceRunner(
                  state,
                  r =>
                    r.copy(
                      activeSubmenu = r.activeSubmenu.map(s => s.copy(editingText = s.editingText.dropRight(1))),
                      statusMessage = None
                    )
                )
              )
            case Some(submenu) if submenu.searchTerm.nonEmpty =>
              ReducerResult.noEffects(replaceRunner(state, _.updateSubmenuSearch(submenu.searchTerm.dropRight(1))))
            case _ =>
              ReducerResult.noEffects(state)
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
          currentRunner(state).flatMap(_.activeSubmenu) match
            case Some(submenu) if submenu.editingItemId.nonEmpty =>
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
          currentRunner(state).flatMap(_.activeSubmenu) match
            case Some(submenu) if submenu.editingItemId.nonEmpty && submenu.editingText.nonEmpty =>
              ReducerResult.noEffects(
                replaceRunner(
                  state,
                  r =>
                    r.copy(
                      activeSubmenu =
                        r.activeSubmenu.map(s => s.copy(editingText = TextEditing.deleteWordBackward(s.editingText))),
                      statusMessage = None
                    )
                )
              )
            case Some(submenu) if submenu.searchTerm.nonEmpty =>
              ReducerResult.noEffects(
                replaceRunner(state, _.updateSubmenuSearch(TextEditing.deleteWordBackward(submenu.searchTerm)))
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
          currentRunner(state).flatMap(_.activeSubmenu) match
            case Some(submenu) if submenu.editingItemId.nonEmpty && submenu.editingText.nonEmpty =>
              ReducerResult.noEffects(
                replaceRunner(
                  state,
                  r =>
                    r.copy(
                      activeSubmenu =
                        r.activeSubmenu.map(s => s.copy(editingText = TextEditing.deleteWordForward(s.editingText))),
                      statusMessage = None
                    )
                )
              )
            case Some(submenu) if submenu.searchTerm.nonEmpty =>
              ReducerResult.noEffects(
                replaceRunner(state, _.updateSubmenuSearch(TextEditing.deleteWordForward(submenu.searchTerm)))
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
        state.clipboard
          .getOrElse("")
          .filter(char => char != '\r' && char != '\n')
          .foldLeft(ReducerResult.noEffects(state))((result, char) =>
            reduceActive(RunnerInsertChar(char), result.state, registry)
          )

      case RunnerNavigate(Direction.Up) =>
        if submenuHasFocus(state) then ReducerResult.noEffects(replaceRunner(state, _.moveSubmenuSelection(-1)))
        else
          ReducerResult.noEffects(replaceRunner(state, runner => updatePreviewForSelection(runner.moveSelection(-1))))

      case RunnerNavigate(Direction.Down) =>
        if submenuHasFocus(state) then ReducerResult.noEffects(replaceRunner(state, _.moveSubmenuSelection(1)))
        else ReducerResult.noEffects(replaceRunner(state, runner => updatePreviewForSelection(runner.moveSelection(1))))

      case RunnerSelectVisibleItem(index) =>
        val mainFocusedState = state.commandRunnerSurface
          .map(surface => state.copy(focus = Focus.Surface(surface.id)))
          .getOrElse(state)
        ReducerResult.noEffects(
          replaceRunner(
            mainFocusedState,
            runner => updatePreviewForSelection(runner.withSelectedVisibleIndex(index))
          )
        )

      case RunnerSelectSubmenuItem(index) =>
        val submenuFocusedState = state.commandRunnerSubmenuSurface
          .map(surface => state.copy(focus = Focus.Surface(surface.id)))
          .getOrElse(state)
        ReducerResult.noEffects(
          replaceRunner(submenuFocusedState, _.withSelectedFocusedSubmenuIndex(index))
        )

      case RunnerSelectPreviewSubmenuItem(groupId, index) =>
        val submenuFocusedState = state.commandRunnerSubmenuSurface
          .map(surface => state.copy(focus = Focus.Surface(surface.id)))
          .getOrElse(state)
        ReducerResult.noEffects(
          replaceRunner(
            submenuFocusedState,
            runner =>
              runner.copy(
                previewedGroupId = Some(groupId),
                activeSubmenu = Some(CommandRunnerSubmenuState(groupId, selectedIndex = index)),
                submenuSelections = runner.submenuSelections + (groupId -> index)
              )
          )
        )

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

      case RunnerNextCategory =>
        given CommandRegistry = registry
        currentRunner(state) match
          case Some(runner) if runner.searchTerm.isEmpty && !submenuHasFocus(state) =>
            ReducerResult.noEffects(replaceRunner(state, r => updatePreviewForSelection(r.switchCategory(1))))
          case _ =>
            ReducerResult.noEffects(state)

      case RunnerPreviousCategory =>
        given CommandRegistry = registry
        currentRunner(state) match
          case Some(runner) if runner.searchTerm.isEmpty && !submenuHasFocus(state) =>
            ReducerResult.noEffects(replaceRunner(state, r => updatePreviewForSelection(r.switchCategory(-1))))
          case _ =>
            ReducerResult.noEffects(state)

  private def deactivate(state: AppState): AppState =
    state
      .copy(
        uiSurfaces = state.uiSurfaces
          .filterNot(surface => surface.id == SubmenuSurfaceId || state.commandRunnerSurface.exists(_.id == surface.id))
      )
      .popFocus

  private def currentRunner(state: AppState): Option[CommandRunner] =
    state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }

  private def submenuHasFocus(state: AppState): Boolean =
    state.focus == Focus.Surface(SubmenuSurfaceId)

  private def submenuEditing(state: AppState): Boolean =
    currentRunner(state).flatMap(_.activeSubmenu.flatMap(_.editingItemId)).nonEmpty

  private def submenuSearching(state: AppState): Boolean =
    currentRunner(state).flatMap(_.activeSubmenu).exists(_.searchTerm.nonEmpty)

  private def rootEditing(state: AppState): Boolean =
    currentRunner(state).flatMap(_.editingItemId).nonEmpty

  private def clearSubmenuEditMode(state: AppState): AppState =
    replaceRunner(
      state,
      runner =>
        runner.copy(
          activeSubmenu = runner.activeSubmenu.map(_.copy(editingItemId = None, editingText = "")),
          statusMessage = None
        )
    )

  private def clearRootEditMode(state: AppState): AppState =
    replaceRunner(state, _.copy(editingItemId = None, editingText = "", statusMessage = None))

  private def submenuSelectedOption(runner: CommandRunner): Option[CommandSurfaceItem.OptionItem] =
    runner.activeSubmenu.flatMap { submenu =>
      submenu.selectedItemFromAll(runner.submenuItems(submenu.groupId)).collect {
        case option: CommandSurfaceItem.OptionItem =>
          option
      }
    }

  private def updatePreviewForSelection(runner: CommandRunner): CommandRunner =
    runner.selectedItem match
      case Some(group: CommandSurfaceItem.GroupItem) if runner.activeCategory == CommandCategory.Settings =>
        runner.previewGroup(group.id)
      case _ =>
        runner.clearGroupPreview

  private def submitSubmenu(state: AppState): ReducerResult =
    currentRunner(state).flatMap(_.activeSubmenu) match
      case Some(submenu) =>
        val runner = currentRunner(state).get
        submenu.selectedItemFromAll(runner.submenuItems(submenu.groupId)) match
          case Some(_: CommandSurfaceItem.InputItem) if submenu.editingItemId.isEmpty =>
            ReducerResult.noEffects(state)
          case Some(item: CommandSurfaceItem.InputItem) =>
            item.parse(submenu.editingText) match
              case Some(intent) =>
                ReducerResult(
                  state = replaceRunner(
                    state,
                    r =>
                      r.copy(
                        activeSubmenu = r.activeSubmenu.map(_.copy(editingItemId = None, editingText = "")),
                        statusMessage = None
                      )
                  ),
                  effects = List(AppEffect.ExecuteCommand(Command.typed(item.id, item.label, intent, item.category)))
                )
              case None =>
                ReducerResult.noEffects(
                  replaceRunner(state, _.copy(statusMessage = Some(invalidInputMessage(item, submenu.editingText))))
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
      case None =>
        ReducerResult.noEffects(state)

  private def invalidInputMessage(item: CommandSurfaceItem.InputItem, text: String): String =
    val value = if text.trim.isEmpty then "<empty>" else text
    if item.acceptsBindingText then s"Invalid binding: $value"
    else s"Invalid value: $value"

  private def replaceRunner(state: AppState, update: CommandRunner => CommandRunner): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val updatedRunner = update(runner)
            val updatedSurfaces = state.uiSurfaces.flatMap {
              case current if current.id == surface.id =>
                List(current.copy(content = SurfaceContent.CommandPalette(updatedRunner)))
              case current if current.id == SubmenuSurfaceId =>
                Nil
              case other =>
                List(other)
            }
            syncSubmenuSurface(state.copy(uiSurfaces = updatedSurfaces), updatedRunner)
          case _ =>
            state
      case None =>
        state

  private def syncSubmenuSurface(state: AppState, runner: CommandRunner): AppState =
    val baseSurfaces  = state.uiSurfaces.filterNot(_.id == SubmenuSurfaceId)
    val mainSurfaceId = state.commandRunnerSurface.map(_.id).getOrElse(SurfaceId("command-runner"))
    runner.previewOrFocusedGroupId match
      case Some(groupId) =>
        val submenuSurface = UiSurface(
          id = SubmenuSurfaceId,
          content = SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly = runner.activeSubmenu.isEmpty),
          presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
        )
        state.copy(
          uiSurfaces = baseSurfaces :+ submenuSurface,
          focus =
            if runner.activeSubmenu.isDefined then Focus.Surface(SubmenuSurfaceId) else Focus.Surface(mainSurfaceId)
        )
      case None =>
        state.copy(uiSurfaces = baseSurfaces, focus = Focus.Surface(mainSurfaceId))
