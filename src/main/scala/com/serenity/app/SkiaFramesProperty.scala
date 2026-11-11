package com.serenity.app

import com.serenity.config.SkiaFrames
import com.serenity.ui.skia.LayerFrames

/** The layer reads what it is handed from a system property, when its first frame is made, so the configured form is
  * set at startup. A value the user set with `-D` stays in force, as for [[SkiaVsync]].
  */
object SkiaFramesProperty:

  def property(frames: SkiaFrames, properties: Map[String, String]): Option[(String, String)] =
    val chosenByUser = properties.get(LayerFrames.Property).exists(_.trim.nonEmpty)
    Option.unless(chosenByUser)(LayerFrames.Property -> frames.configKey)
