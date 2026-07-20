package com.serenity

import java.awt.Color

import com.serenity.config.{AppConfig, BackgroundStyle, MaterialPreset}
import com.serenity.ui.renderer.SurfaceMaterials
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceMaterialsSpec extends AnyFlatSpec with Matchers:

  "SurfaceMaterials" should "derive crystal material values from the material preset" in {
    val config = AppConfig.default.withMaterialPreset(MaterialPreset.Crystal)

    SurfaceMaterials.panelAlpha(config, Theme.default) shouldBe 0.68f
    SurfaceMaterials.effectiveBlurRadius(config) shouldBe 0.75f
    SurfaceMaterials.glassSheenBackground(config, Theme.default) should not be empty
  }

  it should "preserve low-level background behavior for custom material settings" in {
    val config = AppConfig.default
      .withMaterialPreset(MaterialPreset.Crystal)
      .withBackgroundStyle(BackgroundStyle.Transparent)

    config.materialPreset shouldBe MaterialPreset.Custom
    SurfaceMaterials.panelAlpha(config, Theme.default) shouldBe 0.28f
    SurfaceMaterials.effectiveBlurRadius(config) shouldBe 0.0f
    SurfaceMaterials.glassSheenBackground(config, Theme.default) shouldBe None
  }

  it should "blend sheen colors without changing alpha" in {
    val theme = Theme.default.copy(
      panel = Theme.default.panel.copy(
        background = new Color(10, 20, 30, 120),
        foreground = new Color(110, 120, 130, 220)
      )
    )

    SurfaceMaterials
      .glassSheenBackground(AppConfig.default.withMaterialPreset(MaterialPreset.Crystal), theme)
      .map(_.getAlpha) shouldBe Some(120)
  }
