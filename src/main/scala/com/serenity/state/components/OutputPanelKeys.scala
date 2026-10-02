package com.serenity.state.components

import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.state.models.{SurfaceContent, UiSurface}

/** Project output's keys scroll it. Its `cursor` is the scroll position: the panel shows the lines up to the one the
  * cursor is in, and a cursor at the end of the text follows new output. Up/Down move a line, PageUp/PageDown a page,
  * Home goes to the top and End back to following.
  */
private[components] object OutputPanelKeys:

  def handle(
    event: PanelInputEvent,
    surface: UiSurface,
    text: String,
    cursor: Int,
    visibleRows: Int
  ): Option[ComponentResult] =
    // A trailing newline starts no line of its own, so the last line is the last one with text in it.
    val lineStarts = (0 +: text.indices.filter(text(_) == '\n').map(_ + 1)).filter(_ < text.length.max(1)).toVector
    val lastLine   = lineStarts.size - 1
    val following  = cursor >= text.length
    val anchor     = if following then lastLine else lineStarts.lastIndexWhere(_ <= cursor).max(0)
    val topAnchor  = math.max(0, visibleRows - 1).min(lastLine)
    val page       = math.max(1, visibleRows - 1)

    def scrolledTo(line: Int): Option[ComponentResult] =
      val target     = line.max(topAnchor)
      val nextCursor = if target >= lastLine then text.length else lineStarts(target)
      Option.when(nextCursor != cursor.min(text.length))(
        PanelSurfaces.replaced(surface, SurfaceContent.Terminal(text, nextCursor))
      )

    event match
      case PanelInputEvent.Navigate(Direction.Up)   => scrolledTo(anchor - 1)
      case PanelInputEvent.Navigate(Direction.Down) => scrolledTo(anchor + 1)
      case PanelInputEvent.Page(delta)              => scrolledTo(anchor + delta * page)
      case PanelInputEvent.First                    => scrolledTo(topAnchor)
      case PanelInputEvent.Last                     => scrolledTo(lastLine)
      case _                                        => None
