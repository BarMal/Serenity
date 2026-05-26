package com.serenity.ui.layout

import com.serenity.state.models.{EditorPane, PaneId}

case class Layout(
    editorPanes: Map[PaneId, EditorPane],
    activeEditorPaneId: Option[PaneId]
)

object Layout:

  def initial: Layout =
    val initialPane = EditorPane.empty(PaneId(0))
    Layout(
      editorPanes = Map(PaneId(0) -> initialPane),
      activeEditorPaneId = Some(PaneId(0))
    )

  def empty: Layout =
    Layout(
      editorPanes = Map.empty,
      activeEditorPaneId = None
    )
