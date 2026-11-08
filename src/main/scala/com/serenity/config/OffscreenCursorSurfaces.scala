package com.serenity.config

import java.util.Locale

/** What a surface anchored to the caret -- the command palette, the contextual toolbar, a hover -- does when it opens
  * with the caret scrolled out of its pane (#2570). Whichever is chosen, a surface still open when the caret leaves the
  * pane is clamped inside it.
  */
enum OffscreenCursorSurfaces(val configKey: String):
  /** Scroll the view back to the caret, leaving the caret and any selection where they are. */
  case RevealCursor extends OffscreenCursorSurfaces("reveal-cursor")

  /** Move the caret to the pane's middle row, leaving the view where it is; a selection is dropped. */
  case SummonCursor extends OffscreenCursorSurfaces("summon-cursor")

  /** Leave the view and the caret alone and hang the surface on the pane's edge nearest the caret. */
  case Clamp extends OffscreenCursorSurfaces("clamp")

object OffscreenCursorSurfaces:

  def fromConfigKey(value: String): Option[OffscreenCursorSurfaces] =
    OffscreenCursorSurfaces.values.find(_.configKey == value.trim.toLowerCase(Locale.ROOT))
