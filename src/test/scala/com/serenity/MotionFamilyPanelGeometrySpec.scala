package com.serenity

import java.nio.file.Files

import com.serenity.animation.TransitionKind
import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Panel scale-in/out (issue #1085 phase 1): `PanelGeometry` is a genuinely new [[MotionFamily]] -- its own config
  * surface, not folded into `PinnedPanels`' `transitionOverrides` -- wired through exactly the same generic config
  * plumbing every other family already uses
  * (`SurfaceConfigSchemaKeys`/`SurfaceConfigSchemaParser`/`ConfigGroups.motion` all iterate `MotionFamily.values`, so
  * this needs no bespoke keys of its own). Mirrors `MotionFamilyColumnTransitionsSpec`, the precedent for adding a new
  * family this same way.
  */
class MotionFamilyPanelGeometrySpec extends AnyFlatSpec with Matchers:

  "MotionFamily" should "include PanelGeometry" in {
    MotionFamily.values should contain(MotionFamily.PanelGeometry)
  }

  "MotionConfig.fromLegacy" should "give PanelGeometry a sensible, enabled default" in {
    val configuration = MotionConfig.fromLegacy(SurfaceConfig())

    val family = configuration.families(MotionFamily.PanelGeometry)
    family.enabled shouldBe true
    family.speedScale shouldBe 1.0
  }

  it should "disable PanelGeometry along with every other family under the Reduced preset" in {
    val configuration = MotionConfig.forPreset(MotionPreset.Reduced)

    configuration.effective.family(MotionFamily.PanelGeometry).enabled shouldBe false
  }

  it should "disable PanelGeometry when motion accessibility is Off, regardless of the family's own setting" in {
    val configuration = MotionConfig.fromLegacy(SurfaceConfig()).copy(accessibility = MotionAccessibility.Off)

    configuration.effective.family(MotionFamily.PanelGeometry).enabled shouldBe false
  }

  "the config file format" should "round-trip an explicit panel_geometry setting" in {
    import AppConfigMotionOps.*
    val configured = AppConfig.default.withMotionFamilyConfiguration(
      MotionFamily.PanelGeometry,
      MotionFamilyConfig(
        transitionKind = TransitionKind.Disabled,
        animation = com.serenity.animation.AnimationConfig.subtle,
        speedScale = 1.5
      )
    )
    val configFile = Files.createTempFile("serenity-panel-geometry-config", ".conf")
    val serialized = ConfigManager.configToString(configured)
    Files.writeString(configFile, serialized)

    serialized should include("motion.family.panel_geometry.enabled = false")
    serialized should include("motion.family.panel_geometry.speed_scale = 1.5")

    val loaded = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))
    val loadedFamily = loaded.surfaceConfig.motionConfiguration
      .getOrElse(fail("expected authoritative motion configuration"))
      .families(MotionFamily.PanelGeometry)

    loadedFamily.enabled shouldBe false
    loadedFamily.speedScale shouldBe 1.5
  }

  "AppConfig.scaledPanelGeometryAnimation" should "be defined under the default (Smooth) preset" in {
    import AppConfigMotionOps.*
    AppConfig.default.scaledPanelGeometryAnimation shouldBe defined
  }

  it should "be None once motion accessibility is turned off, regardless of the family's own setting" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionAccessibility(MotionAccessibility.Off)
    config.scaledPanelGeometryAnimation shouldBe None
  }

  it should "be None under the Reduced motion preset" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionPreset(MotionPreset.Reduced)
    config.scaledPanelGeometryAnimation shouldBe None
  }

  it should "be independent of the PinnedPanels colour-fade family: turning that family off leaves this one enabled" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionFamilyConfiguration(
      MotionFamily.PinnedPanels,
      AppConfig.default.surfaceConfig.effectiveMotionConfiguration
        .family(MotionFamily.PinnedPanels)
        .disabled
    )

    config.scaledPanelGeometryAnimation shouldBe defined
  }
