package com.serenity.state.models

/** Shared helpers for the "find the item matching a predicate (typically an id) and replace it" shape duplicated across
  * `state/reducers`, `StateManager*` capability files, and `state/components` (issue #1419). Two variants exist because
  * call sites genuinely differ on whether the replaced item should keep its position or move to the end of the list:
  * for `Runtime.uiSurfaces` specifically, list order doubles as z-order (`AppState.modalSurface` reads
  * `uiSurfaces.reverse.find`, and `StateManagerSurfaceCapability`'s pin/update flows rely on
  * `pinnedSurfaces.reverse.find` to mean "most recently touched"), so moving a replaced surface to the end is a
  * deliberate "bring to front" -- not an oversight to unify away. This only collects the two shapes under one name
  * each; it does not change which call sites use which.
  */
extension [A](items: List[A])
  /** Replaces the item matching `matches` with `f` applied to it, keeping its original position. A no-op if no item
    * matches.
    */
  def replacedWhere(matches: A => Boolean)(f: A => A): List[A] =
    items.map(a => if matches(a) then f(a) else a)

  /** Drops the item matching `matches` (if any) and appends `replacement` -- the existing "remove old, append new"
    * idiom, made explicit. Unlike `replacedWhere`, this always moves `replacement` to the end of the list.
    */
  def movedToEndWhere(matches: A => Boolean)(replacement: A): List[A] =
    items.filterNot(matches) :+ replacement
