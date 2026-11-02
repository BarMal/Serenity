package com.serenity.app

/** Skiko reads whether to wait for the display's vertical blank from a system property, and from an environment
  * variable when the property is absent. A value the user set that way stays in force; otherwise the config's
  * `ui.render.skia_vsync` decides. The property is read when the first layer is made, so it is set at startup.
  */
object SkiaVsync:

  val Property: String            = "skiko.vsync.enabled"
  val EnvironmentVariable: String = "SKIKO_VSYNC_ENABLED"

  def property(enabled: Boolean, env: Map[String, String], properties: Map[String, String]): Option[(String, String)] =
    val chosenByUser =
      properties.get(Property).exists(_.trim.nonEmpty) || env.get(EnvironmentVariable).exists(_.trim.nonEmpty)
    Option.unless(chosenByUser)(Property -> enabled.toString)
