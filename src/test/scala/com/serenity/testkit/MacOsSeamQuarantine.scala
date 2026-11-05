package com.serenity.testkit

import java.util.Locale

/** The macOS pane-edge seam is quarantined on macOS only (#2481): a damaged frame differs from a cold one there by an
  * antialiased edge composited twice, which Linux and Windows glyphs do not show. The PR that fixes the seam removes
  * this object and every use of it.
  */
object MacOsSeamQuarantine:

  val Reason: String =
    "Quarantined on macOS only: a copied row's antialiased seam is composited twice there " +
      "(https://github.com/BarMal/Serenity/issues/2481). Removed by the PR that fixes the seam."

  def appliesOn(osName: String): Boolean = osName.toLowerCase(Locale.ROOT).startsWith("mac")

  def appliesHere: Boolean = appliesOn(sys.props.getOrElse("os.name", ""))
