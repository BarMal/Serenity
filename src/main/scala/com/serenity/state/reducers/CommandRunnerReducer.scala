package com.serenity.state.reducers

import com.serenity.command.{CommandRegistry, CommandSurfaceItem, CommandRunner}
import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, Focus, PaneId, SurfaceContent, SurfacePlacement, SurfacePresentation, UiSurface}

object CommandRunnerReducer:

  def reducer(registry: CommandRegistry): Reducer[CommandRunnerEvent] =
    Reducer.instance((event, state) => reduce(event, state, registry))

  def reduce(event: Event, state: AppState, registry: CommandRegistry): ReducerResult =
    CommandRunnerEvent.fromEvent(event)
      .map(reduce(_, state, registry))
      .getOrElse(ReducerResult.noEffects(state))

  def reduce(event: CommandRunnerEvent, state: AppState, registry: CommandRegistry): ReducerResult =
    if currentRunner(state).exists(_.isActive) then reduceActive(event, state, registry)
    else ReducerResult.noEffects(state)

  private def reduceActive(event: CommandRunnerEvent, state: AppState, registry: CommandRegistry): ReducerResult =
    event match
      case RunnerDismiss =>
        ReducerResult.noEffects(deactivate(state))

      case RunnerSubmit =>
        currentRunner(state).flatMap(_.selectedItem) match
          case Some(CommandSurfaceItem.CommandItem(command)) =>
            val previousFocus = currentRunner(state).flatMap(_.previousFocus).getOrElse(Focus.EditorPane(PaneId(0)))
            ReducerResult(
              state = deactivate(state).copy(focus = previousFocus),
              effects = List(AppEffect.ExecuteCommand(command))
            )
          case Some(option: CommandSurfaceItem.OptionItem) =>
            option.selectedIntent match
              case Some(intent) =>
                ReducerResult(
                  state = state,
                  effects = List(AppEffect.ExecuteCommand(com.serenity.command.Command.typed(option.id, option.label, intent, option.category)))
                )
              case None =>
                ReducerResult.noEffects(state)
          case None =>
            ReducerResult.noEffects(deactivate(state))

      case RunnerInsertChar(char) =>
        given CommandRegistry = registry
        ReducerResult.noEffects(replaceRunner(state, runner => runner.updateSearchTerm(runner.searchTerm + char)))

      case RunnerDeleteBackward =>
        if currentRunner(state).exists(_.searchTerm.nonEmpty) then
          given CommandRegistry = registry
          ReducerResult.noEffects(replaceRunner(state, runner => runner.updateSearchTerm(runner.searchTerm.dropRight(1))))
        else ReducerResult.noEffects(state)

      case RunnerNavigate(Direction.Up) =>
        given CommandRegistry = registry
        ReducerResult.noEffects(replaceRunner(state, _.moveSelection(-1)))

      case RunnerNavigate(Direction.Down) =>
        given CommandRegistry = registry
        ReducerResult.noEffects(replaceRunner(state, _.moveSelection(1)))

      case RunnerNavigate(Direction.Left) =>
        given CommandRegistry = registry
        currentRunner(state) match
          case Some(runner) if runner.searchTerm.isEmpty =>
            runner.selectedItem match
              case Some(_: CommandSurfaceItem.OptionItem) =>
                val updatedRunner = runner.adjustSelectedOption(-1)
                val effects = updatedRunner.selectedItem match
                  case Some(option: CommandSurfaceItem.OptionItem) =>
                    option.selectedIntent.toList.map(intent =>
                      AppEffect.ExecuteCommand(com.serenity.command.Command.typed(option.id, option.label, intent, option.category))
                    )
                  case _ => Nil
                ReducerResult(replaceRunner(state, _ => updatedRunner), effects)
              case _ =>
                ReducerResult.noEffects(state)
          case _ =>
            ReducerResult.noEffects(state)

      case RunnerNavigate(Direction.Right) =>
        given CommandRegistry = registry
        currentRunner(state) match
          case Some(runner) if runner.searchTerm.isEmpty =>
            runner.selectedItem match
              case Some(_: CommandSurfaceItem.OptionItem) =>
                val updatedRunner = runner.adjustSelectedOption(1)
                val effects = updatedRunner.selectedItem match
                  case Some(option: CommandSurfaceItem.OptionItem) =>
                    option.selectedIntent.toList.map(intent =>
                      AppEffect.ExecuteCommand(com.serenity.command.Command.typed(option.id, option.label, intent, option.category))
                    )
                  case _ => Nil
                ReducerResult(replaceRunner(state, _ => updatedRunner), effects)
              case _ =>
                ReducerResult.noEffects(state)
          case _ =>
            ReducerResult.noEffects(state)

      case RunnerNextCategory =>
        given CommandRegistry = registry
        currentRunner(state) match
          case Some(runner) if runner.searchTerm.isEmpty =>
            ReducerResult.noEffects(replaceRunner(state, _.switchCategory(1)))
          case _ =>
            ReducerResult.noEffects(state)

      case RunnerPreviousCategory =>
        given CommandRegistry = registry
        currentRunner(state) match
          case Some(runner) if runner.searchTerm.isEmpty =>
            ReducerResult.noEffects(replaceRunner(state, _.switchCategory(-1)))
          case _ =>
            ReducerResult.noEffects(state)

  private def activate(state: AppState, registry: CommandRegistry): AppState =
    val activatedRunner = CommandRunner.empty
      .activate(registry)
      .withPreviousFocus(state.focus)
    val (stateWithId, surfaceId) =
      state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.CommandPalette(activatedRunner),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    stateWithId.copy(
      uiSurfaces = upsertSurface(stateWithId.uiSurfaces, surface),
      focus = Focus.Surface(surfaceId)
    )

  private def deactivate(state: AppState): AppState =
    val previousFocus = currentRunner(state).flatMap(_.previousFocus).getOrElse(Focus.EditorPane(PaneId(0)))
    state.commandRunnerSurface match
      case Some(surface) =>
        state.copy(
          uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id),
          focus = previousFocus
        )
      case None =>
        state.copy(focus = previousFocus)

  private def currentRunner(state: AppState): Option[CommandRunner] =
    state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }

  private def replaceRunner(state: AppState, update: CommandRunner => CommandRunner): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        val updatedSurface = surface.copy(content =
          surface.content match
            case SurfaceContent.CommandPalette(runner) => SurfaceContent.CommandPalette(update(runner))
            case other                                 => other
        )
        state.copy(uiSurfaces = upsertSurface(state.uiSurfaces, updatedSurface))
      case None =>
        state

  private def upsertSurface(surfaces: List[UiSurface], surface: UiSurface): List[UiSurface] =
    surfaces.filterNot(_.id == surface.id) :+ surface
