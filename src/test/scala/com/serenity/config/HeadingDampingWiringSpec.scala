package com.serenity.config

import com.serenity.ui.fonts.FontLoader.{FontConfig, TextScaleMode}
import com.serenity.ui.theme.{HeadingScale, HeadingSettings, RichTextStyling}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeadingDampingWiringSpec extends AnyFlatSpec with Matchers:

  private val baseline = RichTextStyling.ProseZoomBaselinePx
  private val h1Scale  = 22.0f / baseline

  private def withFonts(size: Float, mode: TextScaleMode, multiplier: Double): AppConfig =
    AppConfig.default.withFontConfig(
      FontConfig(textFontSize = size, textScaleMode = mode, textScaleMultiplier = multiplier)
    )

  private def h1Size(config: AppConfig): Option[Float] = config.headingSettings.textStyle(1).fontSize

  private def damped(multiple: Float, bodyPt: Float): Option[Float] =
    Some(HeadingScale.effective(multiple, bodyPt, 24.0f, dampingOn = true) * baseline)

  "The heading settings of a config" should "damp H1 at a 36pt prose body" in {
    h1Size(withFonts(36.0f, TextScaleMode.Auto, 1.0)) shouldBe damped(h1Scale, 36.0f)
    h1Size(withFonts(36.0f, TextScaleMode.Auto, 1.0)).exists(_ < 22.0f * 3.0f) shouldBe true
  }

  it should "count a manual text scale as part of the logical body" in {
    h1Size(withFonts(12.0f, TextScaleMode.Manual, 3.0)) shouldBe damped(h1Scale, 36.0f)
    withFonts(12.0f, TextScaleMode.Manual, 3.0).headingSettings.logicalBodyPt shouldBe 36.0f
  }

  it should "not damp a 12pt body on a hi-DPI display, however the device scale is detected" in
    List(TextScaleMode.Auto, TextScaleMode.Off).foreach { mode =>
      withClue(mode.toString) {
        h1Size(withFonts(12.0f, mode, 3.0)) shouldBe Some(22.0f)
        withFonts(12.0f, mode, 3.0).headingSettings.logicalBodyPt shouldBe 12.0f
      }
    }

  it should "leave the sizes alone at the threshold itself" in {
    h1Size(withFonts(24.0f, TextScaleMode.Auto, 1.0)) shouldBe Some(22.0f)
  }

  it should "skip damping when it is switched off" in {
    val config = withFonts(36.0f, TextScaleMode.Auto, 1.0).withHeadingsConfig(HeadingsConfig(damping = false))

    h1Size(config) shouldBe Some(22.0f)
  }

  it should "damp from the configured threshold" in {
    val config =
      withFonts(36.0f, TextScaleMode.Auto, 1.0).withHeadingsConfig(HeadingsConfig(dampingThresholdPt = 40.0f))

    h1Size(config) shouldBe Some(22.0f)
  }

  it should "damp a configured size like any other" in {
    val config = withFonts(36.0f, TextScaleMode.Auto, 1.0)
      .withHeadingsConfig(HeadingsConfig().withLevel(1)(_.copy(size = Some(3.0f))))

    h1Size(config) shouldBe damped(3.0f, 36.0f)
  }

  it should "be built once per config, and be the shared default for the default config" in {
    val config = withFonts(36.0f, TextScaleMode.Auto, 1.0)

    config.headingSettings should be theSameInstanceAs config.headingSettings
    AppConfig.default.headingSettings should be theSameInstanceAs HeadingSettings.default
  }
