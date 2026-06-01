package com.serenity.ui.layout

import com.serenity.state.models.{EditorPane, PaneId}

case class Layout(
    editorPanes: Map[PaneId, EditorPane],
    activeEditorPaneId: Option[PaneId],
    paneOrder: List[PaneId] = Nil
):

  def orderedPaneIds: List[PaneId] =
    if paneOrder.nonEmpty then paneOrder
    else editorPanes.keys.toList.sortBy(_.value)

object Layout:

  def initial: Layout =
    val initialPane = EditorPane.empty(PaneId(0))
    Layout(
      editorPanes = Map(PaneId(0) -> initialPane),
      activeEditorPaneId = Some(PaneId(0)),
      paneOrder = List(PaneId(0))
    )

  def empty: Layout =
    Layout(
      editorPanes = Map.empty,
      activeEditorPaneId = None,
      paneOrder = Nil
    )
