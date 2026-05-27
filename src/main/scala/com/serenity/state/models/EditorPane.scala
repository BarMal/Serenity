package com.serenity.state.models

import cats.Order

case class PaneId(value: Int)

object PaneId:
  given Order[PaneId] = Order.by(_.value)

case class SmoothScrollState(
    targetTopLine: Int,
    progress: Double
)

case class EditorPane(
    id: PaneId,
    bufferId: Option[BufferId],
    viewport: Viewport,
    cursors: List[CursorPosition],
    centerLine: Int,
    smoothScrolling: Option[SmoothScrollState] = None,
    syncedScrolling: Boolean = false,
    minimapVisible: Boolean = false
)

object EditorPane:

  def empty(id: PaneId): EditorPane =
    EditorPane(
      id = id,
      bufferId = None,
      viewport = Viewport.default,
      cursors = List(CursorPosition(0, 0)),
      centerLine = 0
    )

  def withBuffer(id: PaneId, bufferId: BufferId): EditorPane =
    EditorPane(
      id = id,
      bufferId = Some(bufferId),
      viewport = Viewport.default,
      cursors = List(CursorPosition(0, 0)),
      centerLine = 0
    )
