package com.serenity.ui.renderer

import java.util.concurrent.atomic.AtomicReference

/** Receives the image a layer surface hands back when it flushes. */
final private[renderer] class LayerCapture:
  private val captured = new AtomicReference[Option[RenderImage]](None)

  val onFlush: RenderImage => Unit = image => captured.set(Some(image))

  def image: Option[RenderImage] = captured.get()

private[renderer] object LayerCapture:
  def apply(): LayerCapture = new LayerCapture
