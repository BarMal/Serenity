package com.serenity.state.models

/** A scrollbar thumb held by the pointer: the pane it scrolls, and how far below the thumb's start the pointer took
  * hold of it, which the drag keeps constant so the thumb follows the pointer rather than jumping to it. Lives in
  * `Runtime.pointerGesture.scrollbarGrab` from a press on the thumb until the next press, there being no release event
  * (see [[TabDragSession]]).
  */
final case class ScrollbarGrab(paneId: PaneId, grabOffsetPx: Int)
