package com.serenity.testkit

import java.util.Locale

/** The macOS box-drawing text-row seam is quarantined on macOS only (#2634): a damaged frame differs from a cold one
  * there at a glyph seam of redrawn rows, which Linux and Windows glyphs do not show. The PR that fixes it removes this
  * object and every use of it.
  */
object MacOsSeamQuarantine:

  val Reason: String =
    "Quarantined on macOS only: a redrawn text row's glyph seam is brighter than a frame drawn from scratch there " +
      "(https://github.com/BarMal/Serenity/issues/2634). Removed by the PR that fixes it."

  def appliesOn(osName: String): Boolean = osName.toLowerCase(Locale.ROOT).startsWith("mac")

  def appliesHere: Boolean = appliesOn(sys.props.getOrElse("os.name", ""))
