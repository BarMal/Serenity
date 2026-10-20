package com.serenity.ui.layout

final case class ScreenPosition(x: Int, y: Int)

object CursorLayout:

  def contentRectForPane(paneRect: LayoutRect): LayoutRect =
    EditorPaneLayoutEngine.contentRectForPane(paneRect)
