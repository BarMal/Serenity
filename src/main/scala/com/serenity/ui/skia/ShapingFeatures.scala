package com.serenity.ui.skia

import org.jetbrains.skia.FontFeature
import org.jetbrains.skia.shaper.ShapingOptions

/** The OpenType features Skia shapes a font with, chosen by the font spec's `ligatures`: with it off the font's
  * ligatures and contextual alternates are not applied, as AWT applied none without `LIGATURES_ON`. The glyphs a
  * surface draws and the glyphs layout measures both take their options from here, so they cannot disagree.
  */
private[skia] object ShapingFeatures:

  private val Off = Array("liga", "clig", "calt").map(tag => new FontFeature(tag, false))

  private val Defaults = ShapingOptions.Companion.getDEFAULT()

  private val WithoutLigatures = Defaults.withFeatures(Off)

  def optionsFor(ligatures: Boolean): ShapingOptions = if ligatures then Defaults else WithoutLigatures
