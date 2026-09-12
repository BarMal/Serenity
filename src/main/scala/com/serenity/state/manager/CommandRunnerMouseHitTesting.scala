package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.{CommandRegistry, CommandRunnerSurface, CommandSurfaceItem, SettingsSurfaceState}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.*

/** State the event pipeline exposes for hovering and clicking the command palette and its submenus, as a capability
  * record rather than a trait -- nothing here breaks a construction-order cycle (#1389), so mockability is the only
  * reason this needs an interface at all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class CommandRunnerMouseHitTestingPort(
    stateRef: Ref[IO, AppState],
    applyReducerResult: (ReducerResult, AppState) => IO[Unit]
)

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
        val scene = AuthoritativeUiScene.forState(state, viewportSize)
        surfaces.view
          .flatMap(surface => commandRunnerSelectionForSurface(event, surface, scene, state))
          .headOption
      }

  /** Resolves a hover/click against the open command palette (or its drilled-in settings group). The sub-cell
    * fractional-pixel path (`event.pixelX`/`pixelY` defined -- `CommandRunnerMouseSpec`'s "fractional floating pixel
    * offset" tests) is a distinct, already-tested feature this migration does not touch, so it still goes through
    * `MouseHitTestGeometry.overlayItemIndex`/`FloatingSurfaceGeometry` unchanged. Every other (cell-coordinate) hit
    * resolves through `CommandRunnerSurfaceComposition`'s own `hitAt` instead -- the same resolved plan the renderer
    * paints from (issue #819, slice 2), so a row can no longer be painted at one position and hit-tested at another.
    */
  private def commandRunnerSelectionForSurface(
    event: MouseInputEvent,
    surface: UiSurface,
    scene: UiSceneSnapshot,
    state: AppState
  ): Option[CommandRunnerEvent] =
    surface.content match
      case SurfaceContent.CommandPalette(runner) if event.pixelX.isDefined && event.pixelY.isDefined =>
        pixelCommandRunnerSelection(event, surface, runner, scene.calculatedLayout, scene.editorContract, state)
      case SurfaceContent.CommandPalette(runner) =>
        for
          node <- scene.nodesInPaintOrder.find(_.id == SceneNodeId.Surface(surface.id))
          hit <- CommandRunnerSurfaceComposition
            .forRunner(
              runner,
              node.frameRect,
              state.persisted.config.effectiveCommandRunnerItemGapRows,
              SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity),
              showKeyHints = state.persisted.config.surfaceConfig.commandRunnerShowKeyHints
            )
            .hitAt(event.col.toDouble, event.row.toDouble)
          index <- CommandRunnerSurfaceComposition.absoluteIndexOf(hit.focusId)
        yield runner.surface match
          case CommandRunnerSurface.Settings(_, drilled) if drilled.nonEmpty => RunnerSelectSubmenuItem(index)
          case _                                                             => RunnerSelectVisibleItem(index)
      case _ =>
        None

  private def pixelCommandRunnerSelection(
    event: MouseInputEvent,
    surface: UiSurface,
    runner: com.serenity.command.CommandRunner,
    layout: CalculatedLayout,
    contract: EditorLayoutContract,
    state: AppState
  ): Option[CommandRunnerEvent] =
    contract.overlayContentRect(surface.id).flatMap { contentRect =>
      val rowSlots = contract.overlayRowSlots(surface.id)
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
              itemGapRows = state.persisted.config.effectiveCommandRunnerItemGapRows,
              itemTargetRows =
                SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
            )
            .map { index =>
              if drilled.nonEmpty then RunnerSelectSubmenuItem(index)
              else RunnerSelectVisibleItem(index)
            }
        case CommandRunnerSurface.Palette(_) =>
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
              itemGapRows = state.persisted.config.effectiveCommandRunnerItemGapRows,
              itemTargetRows =
                SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
            )
            .map(RunnerSelectVisibleItem(_))
    }

  /** How many rows `SurfaceContentResolver`'s capped, expand-in-place group preview reserves under the selected row
    * (issue #1059), so hit-testing lands on the right item despite those extra rows shifting everything after them.
    * Only the pixel-mode fallback above still needs this -- `CommandRunnerSurfaceComposition` computes its own window
    * and reservation internally, from the same inputs it paints with.
    */
  private def groupPreviewRowCount(items: List[CommandSurfaceItem], selectedIndex: Int): Int =
    val preview = SettingsSurfaceState.previewRows(items, selectedIndex)
    preview.rows.size + (if preview.overflowCount > 0 then 1 else 0)
