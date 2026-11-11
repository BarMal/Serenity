package com.serenity.ui.layout

object CursorLayout:

  def contentRectForPane(paneRect: LayoutRect): LayoutRect =
    EditorPaneLayoutEngine.contentRectForPane(paneRect)
