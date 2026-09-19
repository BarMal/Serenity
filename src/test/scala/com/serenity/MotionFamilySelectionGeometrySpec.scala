package com.serenity

import java.nio.file.Files

import com.serenity.animation.TransitionKind
import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Selection grow/settle (issue #1085 phase 3): `SelectionGeometry` is a genuinely new [[MotionFamily]] -- its own
  * config surface, not folded into `Cursor` -- wired through exactly the same generic config plumbing every other
  * family already uses (`ConfigGroups.motion` iterates `MotionFamily.values`, so this needs no bespoke keys of its
  * own). Mirrors `MotionFamilyPanelGeometrySpec`, the precedent for adding a new family this same way.
  */
class MotionFamilySelectionGeometrySpec extends AnyFlatSpec with Matchers:

  "MotionFamily" should "include SelectionGeometry" in {
    MotionFamily.values should contain(MotionFamily.SelectionGeometry)
  }

  "MotionConfig.fromLegacy" should "give SelectionGeometry a sensible, enabled default" in {
    val configuration = MotionConfig.fromLegacy(SurfaceConfig())

    val family = configuration.families(MotionFamily.SelectionGeometry)
    family.enabled shouldBe true
    family.speedScale shouldBe 1.0
  }

  it should "disable SelectionGeometry along with every other family under the Reduced preset" in {
    val configuration = MotionConfig.forPreset(MotionPreset.Reduced)

    configuration.effective.family(MotionFamily.SelectionGeometry).enabled shouldBe false
  }

  it should "disable SelectionGeometry when motion accessibility is Off, regardless of the family's own setting" in {
    val configuration = MotionConfig.fromLegacy(SurfaceConfig()).copy(accessibility = MotionAccessibility.Off)

    configuration.effective.family(MotionFamily.SelectionGeometry).enabled shouldBe false
  }

  "the config file format" should "round-trip an explicit selection_geometry setting" in {
    import AppConfigMotionOps.*
    val configured = AppConfig.default.withMotionFamilyConfiguration(
      MotionFamily.SelectionGeometry,
      MotionFamilyConfig(
        transitionKind = TransitionKind.Disabled,
        animation = com.serenity.animation.AnimationConfig.subtle,
        speedScale = 1.5
      )
    )
    val configFile = Files.createTempFile("serenity-selection-geometry-config", ".conf")
    val serialized = ConfigManager.configToString(configured)
    Files.writeString(configFile, serialized)

    serialized should include("motion.family.selection_geometry.enabled = false")
    serialized should include("motion.family.selection_geometry.speed_scale = 1.5")

    val loaded = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))
    val loadedFamily = loaded.surfaceConfig.motionConfiguration
      .getOrElse(fail("expected authoritative motion configuration"))
      .families(MotionFamily.SelectionGeometry)

    loadedFamily.enabled shouldBe false
    loadedFamily.speedScale shouldBe 1.5
  }

  "AppConfig.scaledSelectionGeometryAnimation" should "be defined under the default (Smooth) preset" in {
    import AppConfigMotionOps.*
    AppConfig.default.scaledSelectionGeometryAnimation shouldBe defined
  }

  it should "be None once motion accessibility is turned off, regardless of the family's own setting" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionAccessibility(MotionAccessibility.Off)
    config.scaledSelectionGeometryAnimation shouldBe None
  }

  it should "be None under the Reduced motion preset" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionPreset(MotionPreset.Reduced)
    config.scaledSelectionGeometryAnimation shouldBe None
  }

  it should "be independent of the Cursor glide family: turning that family off leaves this one enabled" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionFamilyConfiguration(
      MotionFamily.Cursor,
      AppConfig.default.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.Cursor).disabled
    )

    config.scaledSelectionGeometryAnimation shouldBe defined
  }
