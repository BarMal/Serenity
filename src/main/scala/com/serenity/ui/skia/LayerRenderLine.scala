package com.serenity.ui.skia

/** The one `[RENDER]` line a layer logs after its first draw. The device is the name Skiko gives its analytics hook for
  * the graphics adapter, so a bench log shows whether the layer ran on a real GPU or a software renderer.
  */
private[skia] object LayerRenderLine:

  def of(renderApi: String, device: Option[String]): String =
    val name = device.map(_.linesIterator.map(_.trim).filter(_.nonEmpty).mkString(" ")).filter(_.nonEmpty)
    s"[RENDER] skia layer renderApi=$renderApi device=${name.getOrElse("unknown")}"
