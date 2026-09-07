package com.serenity.state.models

final case class NavigationPoint(
    paneId: PaneId,
    bufferId: BufferId,
    cursor: CursorPosition
)

final case class HoveredEditorTarget(
    paneId: PaneId,
    bufferId: BufferId,
    cursor: CursorPosition
)

/** Jump-history navigation: every write moves an entry between `backStack` and `forwardStack`. */
final case class NavigationHistory(
    backStack: List[NavigationPoint] = Nil,
    forwardStack: List[NavigationPoint] = Nil
)
