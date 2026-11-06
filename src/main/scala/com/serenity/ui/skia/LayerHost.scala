package com.serenity.ui.skia

import com.serenity.ui.terminal.LayerPresenter
import org.jetbrains.skia.{Image, Picture}

/** What the window canvas shows through a GPU layer: the latest recorded frame, with the carets drawn over it. */
private[ui] trait LayerHost extends LayerPresenter:

  def publish(picture: Picture): Unit

  /** The frame as pixels, which the host draws as they are; it closes the image once a later one has replaced it. */
  def publishImage(image: Image): Unit
