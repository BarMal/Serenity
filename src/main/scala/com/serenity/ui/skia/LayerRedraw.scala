package com.serenity.ui.skia

import org.jetbrains.skiko.SkiaLayer

/** How a hosted layer is asked to draw: after a frame was published to it, and after the carets over it changed. */
private[skia] trait LayerRedraw:

  def afterPublish(): Unit

  def afterCaretsChanged(): Unit

private[skia] object LayerRedraw:

  def over(repaint: () => Unit): LayerRedraw = new LayerRedraw:
    def afterPublish(): Unit       = repaint()
    def afterCaretsChanged(): Unit = repaint()

  def repainting(layer: SkiaLayer): LayerRedraw = over(() => layer.repaint())
