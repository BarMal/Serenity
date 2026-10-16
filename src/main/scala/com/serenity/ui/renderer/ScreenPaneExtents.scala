package com.serenity.ui.renderer

import com.serenity.state.models.PaneId
import com.serenity.ui.layout.PixelRect

/** Each pane's rows as a screen last showed them against as they are now, to find the bands a repaint bounded to the
  * rows of a frame would otherwise leave showing rows the pane no longer has.
  */
private[renderer] object ScreenPaneExtents:

  /** `current` as what `screen` shows from now on, with the bands the screen showed rows in that `current` has none. */
  def exchange(screen: ScreenIdentity, current: Map[PaneId, PixelRect])(
    tracked: Map[ScreenIdentity, Map[PaneId, PixelRect]]
  ): (Map[ScreenIdentity, Map[PaneId, PixelRect]], List[PixelRect]) =
    (tracked.updated(screen, current), vacated(tracked.getOrElse(screen, Map.empty), current))

  private def vacated(shown: Map[PaneId, PixelRect], current: Map[PaneId, PixelRect]): List[PixelRect] =
    shown.toList.flatMap { (paneId, was) =>
      val now = current.get(paneId).fold(was.yPx)(_.bottomPx)
      Option.when(was.bottomPx > now)(was.copy(yPx = now, heightPx = was.bottomPx - now))
    }
