package com.serenity

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Caret-glide (issue #1085 phase 2): `AppConfig.scaledCursorGlideAnimation` reuses the existing `Cursor` motion family
  * (already live for blink/breathe cadence via `AppRuntime.cursorIdleInterval`) rather than a new one -- mirrors
  * `MotionFamilyPanelGeometrySpec`, the precedent for a family-gated `scaledXAnimation` accessor.
  */
class MotionFamilyCursorGlideSpec extends AnyFlatSpec with Matchers:

  "AppConfig.scaledCursorGlideAnimation" should "be defined under the default (Smooth) preset" in {
    import AppConfigMotionOps.*
    AppConfig.default.scaledCursorGlideAnimation shouldBe defined
  }

  it should "be None once motion accessibility is turned off, regardless of the family's own setting" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionAccessibility(MotionAccessibility.Off)
    config.scaledCursorGlideAnimation shouldBe None
  }

  it should "be None under the Reduced motion preset" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionPreset(MotionPreset.Reduced)
    config.scaledCursorGlideAnimation shouldBe None
  }

  it should "be None when the Cursor family's own speed scale is set to zero" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionFamilyConfiguration(
      MotionFamily.Cursor,
      AppConfig.default.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.Cursor).copy(speedScale = 0.0)
    )
    config.scaledCursorGlideAnimation shouldBe None
  }
