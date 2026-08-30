package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.{CommandRegistry, CommandRunnerSurface, CommandSurfaceItem, SettingsSurfaceState}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.*

/** State the event pipeline exposes for hovering and clicking the command palette and its submenus. */
private[manager] trait CommandRunnerMouseHitTestingPort:
  def stateRef: Ref[IO, AppState]
  def applyReducerResult(result: ReducerResult, fallbackState: AppState): IO[Unit]

/** Hit-tests hover/click against the open command palette (or its active submenu) and reduces the resulting
  * `CommandRunnerEvent`, independent of every other mouse target.
  */
final private[manager] class CommandRunnerMouseHitTesting(port: CommandRunnerMouseHitTestingPort):
  import port.*

  def handleCommandRunnerMouseHover(event: MouseInputEvent, state: AppState): IO[Boolean] =
    commandRunnerSelectionAt(event, state) match
      case Some(selectEvent) =>
        val registry = CommandRegistry.withToggleUI
        applyReducerResult(CommandRunnerReducer.reduce(selectEvent, state, registry), state).map(_ => true)
      case None =>
        IO.pure(false)

  def handleCommandRunnerMouseClick(click: MouseClick, state: AppState): IO[Boolean] =
    commandRunnerSelectionAt(click, state) match
      case Some(selectEvent) =>
        val registry = CommandRegistry.withToggleUI
        val selected = CommandRunnerReducer.reduce(selectEvent, state, registry)
        applyReducerResult(selected, state) >>
          stateRef.get
            .flatMap { selectedState =>
              val submitted = CommandRunnerReducer.reduce(RunnerSubmit, selectedState, registry)
              applyReducerResult(submitted, selectedState)
            }
            .map(_ => true)
      case None =>
        IO.pure(false)

  private def commandRunnerSelectionAt(event: MouseInputEvent, state: AppState): Option[CommandRunnerEvent] =
    val surfaces = state.commandRunnerSurface.toList
    if surfaces.isEmpty then None
    else
      state.runtime.viewportSize.flatMap { viewportSize =>
        val scene    = AuthoritativeUiScene.forState(state, viewportSize)
        val layout   = scene.calculatedLayout
        val contract = scene.editorContract
        surfaces.view
          .flatMap(surface => commandRunnerSelectionForSurface(event, surface, layout, contract, state))
          .headOption
      }

  private def commandRunnerSelectionForSurface(
    event: MouseInputEvent,
    surface: UiSurface,
    layout: CalculatedLayout,
    contract: EditorLayoutContract,
    state: AppState
  ): Option[CommandRunnerEvent] =
    contract.overlayContentRect(surface.id).flatMap { contentRect =>
      val rowSlots = contract.overlayRowSlots(surface.id)
      surface.content match
        case SurfaceContent.CommandPalette(runner) =>
          // Dispatch on `CommandRunnerSurface` (issue #931, Stage 2): a settings group drilled into from the
          // palette's old Settings tab renders on this same surface too (issue #1059), so `Settings(_)` hit-tests
          // the same way as the dedicated Settings surface -- against settingsSurfaceItems/
          // settingsSurfaceSelectedIndex -- covering both entry points exactly as the two conditions this replaced
          // did.
          runner.surface match
            case CommandRunnerSurface.Settings(_, drilled) =>
              val items = runner.settingsSurfaceItems
              MouseHitTestGeometry
                .overlayItemIndex(
                  event,
                  state,
                  layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
                  contentRect,
                  rowSlots,
                  items.length,
                  runner.settingsSurfaceSelectedIndex,
                  hasHeader = true,
                  hasFooter = true,
                  reservedContentRows = groupPreviewRowCount(items, runner.settingsSurfaceSelectedIndex),
                  itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
                  itemTargetRows =
                    SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
                )
                .map { index =>
                  if drilled.nonEmpty then RunnerSelectSubmenuItem(index)
                  else RunnerSelectVisibleItem(index)
                }
            case CommandRunnerSurface.Palette(_) =>
              // Category tabs are retired (issue #931): the header is just the live search box now, nothing there
              // to hit-test as a tab click.
              val items = runner.visibleItems
              MouseHitTestGeometry
                .overlayItemIndex(
                  event,
                  state,
                  layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
                  contentRect,
                  rowSlots,
                  items.length,
                  runner.selectedIndex,
                  hasHeader = true,
                  hasFooter = items.nonEmpty || runner.statusMessage.nonEmpty,
                  reservedContentRows = groupPreviewRowCount(items, runner.selectedIndex),
                  itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
                  itemTargetRows =
                    SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
                )
                .map(RunnerSelectVisibleItem(_))
        case _ =>
          None
    }

  /** How many rows `SurfaceContentResolver`'s capped, expand-in-place group preview reserves under the selected row
    * (issue #1059), so hit-testing lands on the right item despite those extra rows shifting everything after them.
    */
  private def groupPreviewRowCount(items: List[CommandSurfaceItem], selectedIndex: Int): Int =
    val preview = SettingsSurfaceState.previewRows(items, selectedIndex)
    preview.rows.size + (if preview.overflowCount > 0 then 1 else 0)
