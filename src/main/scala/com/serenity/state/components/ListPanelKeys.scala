package com.serenity.state.components

import com.serenity.document.DocumentNavigation
import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.state.models.{AppState, SurfaceContent, UiSurface}
import com.serenity.ui.layout.{Location, Symbol, WrappedLineCache}
import com.serenity.ui.widget.{EndBehaviour, SelectableList, WidgetInput}

/** The outline, comments and diagnostics panels' keys: Up/Down/Home/End/PageUp/PageDown move the highlight, stopping at
  * the ends, and Enter opens the highlighted row in the editor as a click would.
  */
private[components] object ListPanelKeys:

  def handle(
    event: PanelInputEvent,
    surface: UiSurface,
    state: AppState,
    visibleRows: Int,
    wrapCache: WrappedLineCache
  ): Option[ComponentResult] =
    listOf(surface.content, state, wrapCache).flatMap { list =>
      event match
        case PanelInputEvent.Activate =>
          list.highlighted.map(location => ComponentResult.updateState(list.open(_, location)))
        case other =>
          movement(other).flatMap { input =>
            val (moved, _) = list.rows.update(input, visibleRows)
            moved.selectedItem
              .filterNot(location => surfaceHighlight(surface.content).contains(location))
              .map(location => PanelSurfaces.replaced(surface, list.highlighting(location)))
          }
    }

  /** A list panel's rows as locations, which row is highlighted -- its own highlight, or for an outline or comments the
    * entry the cursor is in -- and what opening a row and highlighting one do.
    */
  final private case class ListPanel(
      locations: Vector[Location],
      highlighted: Option[Location],
      highlighting: Location => SurfaceContent,
      open: (AppState, Location) => AppState
  ):

    def rows: SelectableList[Location] =
      SelectableList(
        locations,
        highlighted.map(locations.indexOf).filter(_ >= 0),
        endBehaviour = EndBehaviour.Stop
      )

  private def listOf(content: SurfaceContent, state: AppState, wrapCache: WrappedLineCache): Option[ListPanel] =
    content match
      case SurfaceContent.Outline(symbols, active) =>
        Some(
          ListPanel(
            symbols.map(_.location).toVector,
            active.orElse(cursorEntry(symbols, state)),
            location => SurfaceContent.Outline(symbols, Some(location)),
            PanelLocationNavigation.editorAt(_, _, wrapCache = wrapCache)
          )
        )
      case SurfaceContent.Comments(symbols, active) =>
        Some(
          ListPanel(
            symbols.map(_.location).toVector,
            active.orElse(cursorEntry(symbols, state)),
            location => SurfaceContent.Comments(symbols, Some(location)),
            PanelLocationNavigation.commentAt(_, _, wrapCache = wrapCache)
          )
        )
      case SurfaceContent.Diagnostics(issues, active) =>
        Some(
          ListPanel(
            issues.map(_.location).toVector,
            active,
            location => SurfaceContent.Diagnostics(issues, Some(location)),
            PanelLocationNavigation.editorAt(_, _, wrapCache = wrapCache)
          )
        )
      case _ => None

  private def surfaceHighlight(content: SurfaceContent): Option[Location] =
    content match
      case SurfaceContent.Outline(_, active)     => active
      case SurfaceContent.Comments(_, active)    => active
      case SurfaceContent.Diagnostics(_, active) => active
      case _                                     => None

  /** The entry the cursor is in, which an outline or comments panel highlights until one is picked. */
  private def cursorEntry(symbols: List[Symbol], state: AppState): Option[Location] =
    state.activeCursorPosition.flatMap(DocumentNavigation.currentSymbol(symbols, _)).map(_.location)

  private def movement(event: PanelInputEvent): Option[WidgetInput] =
    event match
      case PanelInputEvent.Navigate(Direction.Up)   => Some(WidgetInput.Up)
      case PanelInputEvent.Navigate(Direction.Down) => Some(WidgetInput.Down)
      case PanelInputEvent.First                    => Some(WidgetInput.First)
      case PanelInputEvent.Last                     => Some(WidgetInput.Last)
      case PanelInputEvent.Page(delta) => Some(if delta < 0 then WidgetInput.PageUp else WidgetInput.PageDown)
      case _                           => None
