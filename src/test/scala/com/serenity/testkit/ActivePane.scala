package com.serenity.testkit

import com.serenity.state.models.AppState
import com.serenity.ui.layout.{CalculatedLayout, EditorPaneLayout, LayoutEngine}

/** The active pane's layout, for specs of one-pane geometry. */
object ActivePane:

  def of(state: AppState, layout: CalculatedLayout): EditorPaneLayout =
    val panes = LayoutEngine.calculateEditorPaneLayouts(state, layout)
    state.persisted.layout.activeEditorPaneId
      .flatMap(panes.get)
      .orElse(panes.values.headOption)
      .getOrElse(throw new AssertionError("expected an editor pane layout"))
