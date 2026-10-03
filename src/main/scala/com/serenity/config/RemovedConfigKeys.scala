package com.serenity.config

/** Settings Serenity no longer has. A config file that still names one loads normally; the key is reported once as
  * removed and otherwise ignored, rather than as an unknown key the user might think they mistyped.
  */
object RemovedConfigKeys:

  private val keys: Set[String] = Set(
    "ui.post_processing",
    "ui.material",
    "ui_material",
    "material.preset",
    "material_preset",
    "ui.shadows",
    "ui_shadows",
    "ui.background_style",
    "ui.background.style",
    "ui_background_style",
    "ui.blur_radius",
    "ui.blur.radius",
    "ui_blur_radius",
    "ui.corner_radius",
    "ui.corner.radius",
    "ui_corner_radius"
  )

  def isRemoved(key: String): Boolean = keys.contains(key)
