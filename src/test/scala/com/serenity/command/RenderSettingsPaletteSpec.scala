package com.serenity.command

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The two renderer choices in the settings palette offer only what Skia can do (#2660 item 1). */
class RenderSettingsPaletteSpec extends AnyFlatSpec with Matchers:

  "The Renderer setting" should "offer Auto, Skia and Skia (CPU), with Auto selected by default" in {
    val item = CommandRunnerSettingsRenderItems.renderBackendOptionItem(Map.empty)

    item.options.map(_.label) shouldBe List("Auto", "Skia", "Skia (CPU)")
    item.selectedIndex shouldBe 0
  }

  "The Wayland Renderer setting" should "offer Skia (CPU) and Skia via XWayland, with Skia (CPU) selected by default" in {
    val item = CommandRunnerSettingsRenderItems.renderWaylandOptionItem(Map.empty)

    item.options.map(_.label) shouldBe List("Skia (CPU)", "Skia via XWayland")
    item.selectedIndex shouldBe 0
  }

end RenderSettingsPaletteSpec
