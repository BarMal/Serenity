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
    "ui_corner_radius",
    "ui.visual_flair",
    "visual.flair.level",
    "window.translucent",
    "window_translucent",
    // `ui.motion` once was a leaf beside `ui.motion.*`; `character.animation` and `character_animation` the same.
    "ui.motion",
    "ui_motion",
    "character.animation",
    "character_animation"
  )

  private val prefixes: List[String] = List(
    "ui.companion_sprite.",
    "companion.sprite.",
    // The motion settings: `motion.*` and its older `ui.motion.*` spelling, the per-family blocks under both, and the
    // character animation keys that sat beside them.
    "motion.",
    "ui.motion.",
    "ui_motion_",
    "motion_",
    "character.animation.",
    "character_animation_"
  )

  def isRemoved(key: String): Boolean = keys.contains(key) || prefixes.exists(key.startsWith)
