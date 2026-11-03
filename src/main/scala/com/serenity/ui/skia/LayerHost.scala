package com.serenity.ui.skia

import com.serenity.ui.terminal.LayerPresenter
import org.jetbrains.skia.Picture

/** What the window canvas shows through a GPU layer: the latest recorded frame, with the carets drawn over it. */
private[ui] trait LayerHost extends LayerPresenter:

  def publish(picture: Picture): Unit
