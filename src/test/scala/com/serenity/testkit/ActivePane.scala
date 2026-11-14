package com.serenity.testkit

import com.serenity.state.models.{AppState, PaneId}
import com.serenity.ui.layout.{CalculatedLayout, EditorPaneLayout, LayoutEngine, LayoutRect}

/** The active pane's layout, for specs of one-pane geometry. */
object ActivePane:

  def of(state: AppState, layout: CalculatedLayout): EditorPaneLayout =
    val panes = LayoutEngine.calculateEditorPaneLayouts(state, layout)
    state.persisted.layout.activeEditorPaneId
      .flatMap(panes.get)
      .orElse(panes.values.headOption)
      .getOrElse(throw new AssertionError("expected an editor pane layout"))

  /** The active pane's text across the workspace's height: what the editor panel was while one pane filled it. */
  def textRect(state: AppState, layout: CalculatedLayout): LayoutRect =
    val pane = of(state, layout)
    LayoutRect(
      pane.contentRect.x,
      layout.editorPanelRect.y,
      pane.contentRect.width + pane.scrollbarRect.width,
      layout.editorPanelRect.height
    )

  /** `paneId`'s pane rect narrowed to its text column, for specs that click or place things by cell. */
  def textColumn(state: AppState, layout: CalculatedLayout, paneId: PaneId): LayoutRect =
    val pane = LayoutEngine.calculateEditorPaneLayouts(state, layout)(paneId)
    pane.paneRect.copy(x = pane.contentRect.x, width = pane.contentRect.width + pane.scrollbarRect.width)
