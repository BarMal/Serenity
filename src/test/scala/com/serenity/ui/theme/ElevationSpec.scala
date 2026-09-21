package com.serenity.ui.theme

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElevationSpec extends AnyFlatSpec with Matchers:

  private val orderedLevels =
    List(ElevationLevel.Base, ElevationLevel.Raised, ElevationLevel.Floating, ElevationLevel.Modal)

  private val themes = List(Theme.dark, Theme.light)

  "ElevationLevel" should "order strictly Base < Raised < Floating < Modal" in {
    orderedLevels.sorted shouldBe orderedLevels
    ElevationLevel.values.toList.sorted shouldBe orderedLevels
  }

  "Theme.elevation" should "derive without any theme file changes, for both dark and light themes" in
    themes.foreach(theme => theme.elevation shouldBe ElevationLevels.derive(theme.foreground, theme.background))

  it should "give strictly increasing shadow opacity from Base to Modal" in
    themes.foreach { theme =>
      val opacities = orderedLevels.map(level => theme.elevation.forLevel(level).shadowOpacity.value)
      opacities.zip(opacities.tail).foreach {
        case (lower, higher) =>
          lower should be < higher
      }
    }

  it should "give Base no surface tint -- the resting layer needs no depth cue" in
    themes.foreach(theme => theme.elevation.base.surfaceTint shouldBe None)

  it should "give a dark theme a surface tint above Base, since a shadow barely reads against a dark backdrop" in {
    val dark = Theme.dark
    dark.elevation.raised.surfaceTint shouldBe defined
    dark.elevation.floating.surfaceTint shouldBe defined
    dark.elevation.modal.surfaceTint shouldBe defined
  }

  it should "keep every shadow opacity within [0, 1] on both dark and light themes" in
    themes.foreach { theme =>
      orderedLevels.foreach { level =>
        val opacity = theme.elevation.forLevel(level).shadowOpacity.value
        opacity should be >= 0.0
        opacity should be <= 1.0
      }
    }
end ElevationSpec
