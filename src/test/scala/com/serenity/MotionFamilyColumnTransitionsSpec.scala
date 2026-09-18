package com.serenity

import java.nio.file.Files

import com.serenity.animation.TransitionKind
import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1 animation): `ColumnTransitions` is a genuinely new
  * [[MotionFamily]] -- its own config surface, not folded into an existing family -- wired through exactly the same
  * generic config plumbing every other family already uses (`SurfaceConfigSchemaKeys`/`SurfaceConfigSchemaParser`/
  * `ConfigGroups.motion` all iterate `MotionFamily.values`, so this needs no bespoke keys of its own).
  */
class MotionFamilyColumnTransitionsSpec extends AnyFlatSpec with Matchers:

  "MotionFamily" should "include ColumnTransitions" in {
    MotionFamily.values should contain(MotionFamily.ColumnTransitions)
  }

  "MotionConfig.fromLegacy" should "give ColumnTransitions a sensible, enabled default" in {
    val configuration = MotionConfig.fromLegacy(SurfaceConfig())

    val family = configuration.families(MotionFamily.ColumnTransitions)
    family.enabled shouldBe true
    family.transitionKind shouldBe TransitionKind.DirectionalSweep
    family.speedScale shouldBe 1.0
  }

  it should "disable ColumnTransitions along with every other family under the Reduced preset" in {
    val configuration = MotionConfig.forPreset(MotionPreset.Reduced)

    configuration.effective.family(MotionFamily.ColumnTransitions).enabled shouldBe false
  }

  it should "disable ColumnTransitions when motion accessibility is Off, regardless of the family's own setting" in {
    val configuration = MotionConfig.fromLegacy(SurfaceConfig()).copy(accessibility = MotionAccessibility.Off)

    configuration.effective.family(MotionFamily.ColumnTransitions).enabled shouldBe false
  }

  "the config file format" should "round-trip an explicit column_transitions setting" in {
    import AppConfigMotionOps.*
    val configured = AppConfig.default.withMotionFamilyConfiguration(
      MotionFamily.ColumnTransitions,
      MotionFamilyConfig(
        transitionKind = TransitionKind.Fade,
        animation = com.serenity.animation.AnimationConfig.subtle,
        speedScale = 1.5
      )
    )
    val configFile = Files.createTempFile("serenity-column-transitions-config", ".conf")
    val serialized = ConfigManager.configToString(configured)
    Files.writeString(configFile, serialized)

    serialized should include("motion.family.column_transitions.transition = fade")
    serialized should include("motion.family.column_transitions.speed_scale = 1.5")

    val loaded = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))
    val loadedFamily = loaded.surfaceConfig.motionConfiguration
      .getOrElse(fail("expected authoritative motion configuration"))
      .families(MotionFamily.ColumnTransitions)

    loadedFamily.transitionKind shouldBe TransitionKind.Fade
    loadedFamily.speedScale shouldBe 1.5
  }

  "AppConfig.scaledColumnTransitionAnimation" should "be defined under the default (Smooth) preset" in {
    import AppConfigMotionOps.*
    AppConfig.default.scaledColumnTransitionAnimation shouldBe defined
  }

  it should "be None once motion accessibility is turned off, regardless of the family's own setting" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionAccessibility(MotionAccessibility.Off)
    config.scaledColumnTransitionAnimation shouldBe None
  }

  it should "be None under the Reduced motion preset" in {
    import AppConfigMotionOps.*
    val config = AppConfig.default.withMotionPreset(MotionPreset.Reduced)
    config.scaledColumnTransitionAnimation shouldBe None
  }
