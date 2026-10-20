package com.serenity.state.models

import com.serenity.config.{AppConfig, ChromeElement, StatusLineConfig, StatusLinePlacement, TabStripMode}

/** The chrome the user has summoned, over what the config shows. `held` is the elements whose chord is down now and
  * `locked` the ones kept shown after the chord was let go. Only `locked` outlives the process: a chord that is down
  * cannot survive a restart.
  */
final case class ChromeSummons(held: Set[ChromeElement] = Set.empty, locked: Set[ChromeElement] = Set.empty):

  def shows(element: ChromeElement): Boolean = held.contains(element) || locked.contains(element)

  /** Pressing the chord of a locked element dismisses it instead, and it stays hidden for the rest of that press. */
  def pressed(element: ChromeElement): ChromeSummons =
    if locked.contains(element) then copy(locked = locked - element) else copy(held = held + element)

  def released(element: ChromeElement): ChromeSummons = copy(held = held - element)

  def releasedAll: ChromeSummons = copy(held = Set.empty)

  def lockHeld: ChromeSummons = copy(locked = locked ++ held)

  /** A terminal cannot report a release, so the chord alone shows and locks, and dismisses on the next press. */
  def toggled(element: ChromeElement): ChromeSummons =
    if shows(element) then copy(held = held - element, locked = locked - element) else copy(locked = locked + element)

object ChromeSummons:
  val empty: ChromeSummons = ChromeSummons()

/** What each chrome element shows, once summoning is taken into account: the config value, or summoned, or locked.
  *
  * This is the one place that is decided. The layout engine reserves room from it and the renderer paints from it, so
  * the two cannot disagree about whether a rail or a row is there.
  */
final case class EffectiveChrome(
    lineNumbers: Boolean,
    paneHeaders: Boolean,
    scrollbar: Boolean,
    tabStrip: Boolean,
    statusLine: StatusLineConfig
)

object EffectiveChrome:

  def of(config: AppConfig, summons: ChromeSummons): EffectiveChrome =
    EffectiveChrome(
      lineNumbers = config.surfaceConfig.showLineNumbers || summons.shows(ChromeElement.LineNumbers),
      paneHeaders = config.surfaceConfig.showPaneHeaders || summons.shows(ChromeElement.PaneHeaders),
      scrollbar = config.surfaceConfig.scrollbarEnabled || summons.shows(ChromeElement.Scrollbar),
      tabStrip = config.tabStripMode == TabStripMode.Auto || summons.shows(ChromeElement.TabStrip),
      statusLine = statusLine(config.statusLine, summons.shows(ChromeElement.StatusBar))
    )

  private def statusLine(configured: StatusLineConfig, summoned: Boolean): StatusLineConfig =
    if !summoned || configured.isShown then configured
    else
      configured.copy(
        placement =
          if configured.placement == StatusLinePlacement.Off then StatusLinePlacement.Pinned else configured.placement,
        segments = if configured.segments.isEmpty then StatusLineConfig.defaultSegments else configured.segments
      )
