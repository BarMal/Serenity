package com.serenity.state.models

/** An in-progress tab-bar drag-to-reorder gesture (issue #1079): the tab a primary-button press picked up, tracked in
  * `Runtime.tabDragSession` only for the lifetime of that gesture -- never persisted, matching every other transient
  * mouse-interaction state (`Runtime.hoveredEditorTarget`, `Runtime.cursorPeekSession`). There is no mouse-release
  * event in this app's input model (`MouseEvent.scala`), so this carries no drop-in-progress position of its own:
  * `TabBarDragHitTesting` reorders `bufferOrder` live on each `MouseDrag` tick instead of deferring to a release this
  * app can never observe, and the next `MousePress` -- the only reliable "the previous gesture ended" signal available
  * -- always resets this to whatever that press starts (or clears it, if the press missed the tab bar).
  */
final case class TabDragSession(bufferId: BufferId)
