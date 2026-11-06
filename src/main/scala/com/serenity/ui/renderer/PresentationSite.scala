package com.serenity.ui.renderer

/** Where a frame is presented, as a backend that must host something there sees it. A window system supplies its own
  * kind of site and the backend that can host on it recognises that kind; any other site is one it cannot, and it draws
  * as the fallback does. No window-system type appears here.
  */
trait PresentationSite

object PresentationSite:

  /** The site of a target with no window system to host anything on. */
  object None extends PresentationSite
