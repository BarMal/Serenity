package com.serenity.state.models

import com.serenity.input.CursorPeekState
import com.serenity.ui.layout.ScreenPosition

/** Transient mouse/pointer interaction state one grab-bag category of `Runtime` used to hold directly (issue #1693):
  * the editor position currently under the pointer (`hoveredEditorTarget`, written by `EditorMouseTargeting.hover`),
  * the in-progress tab-bar drag-to-reorder gesture (`tabDragSession`, see `TabDragSession`'s own doc comment), and the
  * experimental cursor-peek prototype's three fields -- `cursorPeekSession` (`CursorPeekDetector`'s hold-vs-double-tap
  * timing state), `cursorPeekAnchor` (the cursor position frozen at the moment a peek begins), and
  * `cursorPeekResolvedAnchor` (that position resolved to an actual on-screen position exactly once, by
  * `CursorPeekAnchorResolution`). `TabDragSession`'s doc comment already named these five as "every other transient
  * mouse-interaction state" before this grouping existed. None of these is ever persisted, matching every other
  * pointer-driven field here.
  *
  * `pointerShape` is the cursor shape the last mouse move resolved; a drag leaves it alone, so a gesture keeps the
  * shape it began with. `shapeUnderModal` says whether that shape was resolved against a blocking modal's own hit
  * targets (see `PointerShape.shown`).
  */
final case class PointerGestureState(
    hoveredEditorTarget: Option[HoveredEditorTarget] = None,
    cursorPeekSession: CursorPeekState = CursorPeekState.empty,
    cursorPeekAnchor: Option[CursorPosition] = None,
    cursorPeekResolvedAnchor: Option[ScreenPosition] = None,
    tabDragSession: Option[TabDragSession] = None,
    pointerShape: PointerShape = PointerShape.Default,
    shapeUnderModal: Boolean = false
)
