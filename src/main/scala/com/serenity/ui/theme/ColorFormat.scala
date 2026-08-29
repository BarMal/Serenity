package com.serenity.ui.theme

import java.awt.Color

/** Canonical `Color` primitives -- hex-string formatting and alpha-channel overrides -- kept in one place so they don't
  * re-drift across config, session, and document-export code (#1051, #1052).
  */
object ColorFormat:

  /** `#RRGGBB`, uppercase. When `withAlpha` is true and `color` isn't fully opaque, an uppercase alpha byte is appended
    * (`#RRGGBBAA`); a fully-opaque color is never suffixed, and `withAlpha = false` always drops the alpha channel.
    */
  def toHex(color: Color, withAlpha: Boolean): String =
    val rgb = f"#${color.getRed}%02X${color.getGreen}%02X${color.getBlue}%02X"
    if withAlpha && color.getAlpha != 255 then f"$rgb${color.getAlpha}%02X" else rgb

  extension (color: Color)
    /** `color` with its alpha channel replaced by `alpha`, otherwise identical. */
    def withAlpha(alpha: Int): Color =
      if color.getAlpha == alpha then color else new Color(color.getRed, color.getGreen, color.getBlue, alpha)
