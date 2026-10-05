package com.serenity.state.components

import com.serenity.document.DocumentNavigation
import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.state.models.{AppState, SurfaceContent, UiSurface}
import com.serenity.ui.layout.{Location, Symbol, WrappedLineCache}
import com.serenity.ui.widget.{EndBehaviour, ListScroll, SelectableList, WidgetInput}

/** The outline, comments and diagnostics panels' keys: Up/Down/Home/End/PageUp/PageDown move the highlight, stopping at
  * the ends, and bring it back into view if the wheel scrolled it away; Enter opens the highlighted row in the editor
  * as a click would.
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
            val (moved, _) = list.rows(visibleRows).update(input, visibleRows)
            moved.selectedItem
              .map(location => list.highlighting(location, ListScroll(moved.offset)))
              .filterNot(_ == surface.content)
              .map(content => PanelSurfaces.replaced(surface, content))
          }
    }

  /** A list panel's rows as locations, which row is highlighted -- its own highlight, or for an outline or comments the
    * entry the cursor is in -- where it is scrolled to, and what opening a row and highlighting one do.
    */
  final private case class ListPanel(
      locations: Vector[Location],
      highlighted: Option[Location],
      scroll: ListScroll,
      highlighting: (Location, ListScroll) => SurfaceContent,
      open: (AppState, Location) => AppState
  ):

    /** Starts from the rows shown, so a key scrolls only as far as keeping the highlight in view needs. */
    def rows(visibleRows: Int): SelectableList[Location] =
      val selected = highlighted.map(locations.indexOf).filter(_ >= 0)
      SelectableList(
        locations,
        selected,
        offset = scroll.shownOffset(locations.size, selected, visibleRows),
        endBehaviour = EndBehaviour.Stop
      )

  private def listOf(content: SurfaceContent, state: AppState, wrapCache: WrappedLineCache): Option[ListPanel] =
    content match
      case SurfaceContent.Outline(symbols, active, scroll) =>
        Some(
          ListPanel(
            symbols.map(_.location).toVector,
            active.orElse(cursorEntry(symbols, state)),
            scroll,
            (location, shown) => SurfaceContent.Outline(symbols, Some(location), shown),
            PanelLocationNavigation.editorAt(_, _, wrapCache = wrapCache)
          )
        )
      case SurfaceContent.Comments(symbols, active, scroll) =>
        Some(
          ListPanel(
            symbols.map(_.location).toVector,
            active.orElse(cursorEntry(symbols, state)),
            scroll,
            (location, shown) => SurfaceContent.Comments(symbols, Some(location), shown),
            PanelLocationNavigation.commentAt(_, _, wrapCache = wrapCache)
          )
        )
      case SurfaceContent.Diagnostics(issues, active, scroll) =>
        Some(
          ListPanel(
            issues.map(_.location).toVector,
            active,
            scroll,
            (location, shown) => SurfaceContent.Diagnostics(issues, Some(location), shown),
            PanelLocationNavigation.editorAt(_, _, wrapCache = wrapCache)
          )
        )
      case _ => None

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
