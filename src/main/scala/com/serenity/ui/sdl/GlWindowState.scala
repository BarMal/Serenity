package com.serenity.ui.sdl

/** What a GL window knows about itself. Only events change it, so it is a value the window host keeps in a `Ref`. */
final case class GlWindowState(
    pixelWidth: Int,
    pixelHeight: Int,
    scale: Double,
    resizePending: Boolean = false,
    closeRequested: Boolean = false
):

  /** The state after `event`; `scaleNow` is asked only when the display scale changed. */
  def after(event: SdlEvent, scaleNow: => Double): GlWindowState = event match
    case SdlEvent.Quit                                             => copy(closeRequested = true)
    case SdlEvent.Window(SdlEvent.WindowKind.CloseRequested, _, _) => copy(closeRequested = true)
    case SdlEvent.Window(SdlEvent.WindowKind.PixelSizeChanged, width, height)
        if width != pixelWidth || height != pixelHeight =>
      copy(pixelWidth = width, pixelHeight = height, resizePending = true)
    case SdlEvent.Window(SdlEvent.WindowKind.DisplayScaleChanged, _, _) =>
      copy(scale = scaleNow, resizePending = true)
    case _ => this

  /** The pixel size to lay out for, once, until the next change. */
  def takeResize: (GlWindowState, Option[(Int, Int)]) =
    if resizePending then (copy(resizePending = false), Some((pixelWidth, pixelHeight))) else (this, None)
