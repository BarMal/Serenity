package com.serenity.ui.skia

import org.jetbrains.skia.{Canvas, PictureRecorder}

private[skia] object PictureRecording:

  /** Starts recording a `width` x `height` picture with no bounding-box hierarchy, the parameter Kotlin defaults to
    * none and Scala cannot leave out.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def begin(recorder: PictureRecorder, width: Int, height: Int): Canvas =
    recorder.beginRecording(0f, 0f, width.toFloat, height.toFloat, null)
