package com.serenity.config

/** Performance/battery tier for purely decorative visual flourishes (currently: the companion sprite pane, and
  * background blur -- see `SurfaceMaterials.effectiveBlurRadius`). Deliberately separate from [[MotionAccessibility]]:
  * that setting is an accessibility control over motion itself, while this one is a cost control a viewer reaches for
  * on a slow link or a battery-powered machine, independent of whether they want motion reduced.
  */
enum VisualFlairLevel(val configKey: String):
  case Full    extends VisualFlairLevel("full")
  case Reduced extends VisualFlairLevel("reduced")
  case Off     extends VisualFlairLevel("off")

object VisualFlairLevel:

  val default: VisualFlairLevel = Full

  def fromConfigKey(value: String): Option[VisualFlairLevel] =
    value.trim.toLowerCase match
      case "full" | "standard" | "on"  => Some(Full)
      case "reduced"                   => Some(Reduced)
      case "off" | "disabled" | "none" => Some(Off)
      case _                           => None
