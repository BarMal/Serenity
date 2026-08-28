package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.{CommandCategory, CommandRegistry, CommandSurfaceItem}
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
          (selectEvent match
            case _: RunnerSelectCategory => IO.unit
            case _ =>
              stateRef.get.flatMap { selectedState =>
                val submitted = CommandRunnerReducer.reduce(RunnerSubmit, selectedState, registry)
                applyReducerResult(submitted, selectedState)
              }
          ).map(_ => true)
      case None =>
        IO.pure(false)

  private def commandRunnerSelectionAt(event: MouseInputEvent, state: AppState): Option[CommandRunnerEvent] =
    val surfaces =
      event match
        case _: MouseMove
            if state.commandRunnerSubmenuSurface
              .exists(surface => state.persisted.focus == Focus.Surface(surface.id)) =>
          state.commandRunnerSubmenuSurface.toList
        case _ =>
          List(state.commandRunnerSubmenuSurface, state.commandRunnerSurface).flatten
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
          if runner.isSettingsSurface then
            MouseHitTestGeometry
              .overlayItemIndex(
                event,
                state,
                layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
                contentRect,
                rowSlots,
                runner.settingsSurfaceItems.length,
                runner.settingsSurfaceSelectedIndex,
                hasHeader = true,
                hasFooter = true,
                itemGapRows = state.persisted.config.commandRunnerItemGapRows,
                itemTargetRows =
                  SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
              )
              .map { index =>
                if runner.activeSubmenu.nonEmpty then RunnerSelectSubmenuItem(index) else RunnerSelectVisibleItem(index)
              }
          else
            commandPaletteCategoryAt(event, contentRect, contract.overlayHeaderRect(surface.id), runner.searchTerm)
              .map(RunnerSelectCategory(_))
              .orElse(
                MouseHitTestGeometry
                  .overlayItemIndex(
                    event,
                    state,
                    layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
                    contentRect,
                    rowSlots,
                    runner.visibleItems.length,
                    runner.selectedIndex,
                    hasHeader = true,
                    hasFooter = runner.visibleItems.nonEmpty || runner.statusMessage.nonEmpty,
                    itemGapRows = state.persisted.config.commandRunnerItemGapRows,
                    itemTargetRows =
                      SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
                  )
                  .map(RunnerSelectVisibleItem(_))
              )
        case SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly) =>
          val submenuState = runner.activeSubmenu.filter(_.groupId == groupId)
          val items = submenuState
            .map(_.filteredItems(runner.submenuItems(groupId)))
            .getOrElse(runner.submenuItems(groupId))
          val selectedIndex = submenuState.map(_.selectedIndex).getOrElse(0)
          val group         = runner.submenuGroup(groupId)
          val detailRows    = commandRunnerSubmenuDetailRowCount(groupId, items.lift(selectedIndex))
          MouseHitTestGeometry
            .overlayItemIndex(
              event,
              state,
              layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
              contentRect,
              rowSlots,
              items.length,
              selectedIndex,
              hasHeader = group.nonEmpty,
              hasFooter = items.nonEmpty || runner.statusMessage.nonEmpty,
              reservedContentRows = detailRows,
              itemGapRows = state.persisted.config.commandRunnerItemGapRows,
              itemTargetRows =
                SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
            )
            .map { index =>
              if previewOnly then RunnerSelectPreviewSubmenuItem(groupId, index)
              else RunnerSelectSubmenuItem(index)
            }
        case _ =>
          None
    }

  private def commandPaletteCategoryAt(
    event: MouseInputEvent,
    contentRect: LayoutRect,
    headerRect: Option[LayoutRect],
    searchTerm: String
  ): Option[CommandCategory] =
    val categories = CommandCategory.values.toList
    val categoryIndex =
      Option.when(searchTerm.isEmpty && headerRect.exists(_.contains(event.col, event.row))) {
        ((event.col - contentRect.x) * categories.length) / contentRect.width.max(1)
      }
    categoryIndex.flatMap(categories.lift)

  private def commandRunnerSubmenuDetailRowCount(
    groupId: String,
    selectedItem: Option[CommandSurfaceItem]
  ): Int =
    selectedItem.count {
      case group: CommandSurfaceItem.GroupItem
          if groupId == "settings-ui-presets" &&
            (group.id == "settings-preset-create" || group.id == "settings-preset-edit") =>
        true
      case option: CommandSurfaceItem.OptionItem
          if groupId == "settings-preset-select" && option.id == "ui-preset-select" =>
        true
      case _ =>
        false
    }
