package com.serenity

import com.serenity.config.*
import com.serenity.ui.layout.SurfaceFrameLayout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterfaceConfigSpec extends AnyFlatSpec with Matchers:

  "InterfaceConfig" should "own interface density and chrome metric schema metadata" in {
    ConfigKeySchema.isKnownKey("ui.density") shouldBe true
    ConfigKeySchema.isKnownKey("ui.element_gap") shouldBe true
    ConfigKeySchema.isKnownKey("ui.element.gap") shouldBe true
    ConfigKeySchema.isKnownKey("ui.outline_thickness") shouldBe true
    ConfigKeySchema.isKnownKey("ui.outline.thickness") shouldBe true

    ConfigKeySchema.deprecatedKeys.should(
      contain allOf (
        "interface_density"    -> "ui.density",
        "ui_element_gap"       -> "ui.element_gap",
        "ui_outline_thickness" -> "ui.outline_thickness"
      )
    )
  }

  it should "group interface density and chrome metrics under AppConfig" in {
    val config = AppConfig.default.withInterfaceConfig(
      InterfaceConfig(
        density = InterfaceDensity.Spacious,
        elementGap = Some(3),
        outlineThicknessPx = 4
      )
    )

    config.interfaceConfig shouldBe InterfaceConfig(
      density = InterfaceDensity.Spacious,
      elementGap = Some(3),
      outlineThicknessPx = 4
    )
  }

  it should "parse interface density values centrally" in {
    InterfaceDensity.fromConfigKey("compact").shouldBe(Some(InterfaceDensity.Compact))
    InterfaceDensity.fromConfigKey("comfortable").shouldBe(Some(InterfaceDensity.Comfortable))
    InterfaceDensity.fromConfigKey("unknown").shouldBe(None)
  }

  it should "parse interface config entries centrally" in {
    val densityConfig =
      ConfigRegistry
        .read(AppConfig.default, "interface_density", "spacious")
        .getOrElse(fail("density parse"))
    val gapConfig =
      ConfigRegistry.read(AppConfig.default, "ui.element_gap", "4").getOrElse(fail("gap parse"))
    val outlineConfig =
      ConfigRegistry
        .read(AppConfig.default, "ui.outline.thickness", "5")
        .getOrElse(fail("outline parse"))

    densityConfig.interfaceConfig.density.shouldBe(InterfaceDensity.Spacious)
    gapConfig.interfaceConfig.elementGap.shouldBe(Some(4))
    outlineConfig.interfaceConfig.outlineThicknessPx.shouldBe(5)
    ConfigRegistry.read(AppConfig.default, "ui.density", "unknown").shouldBe(None)
  }

  it should "validate interface config entries centrally" in {
    ConfigRegistry.rejects("ui.density", "compact").shouldBe(false)
    ConfigRegistry.rejects("ui.density", "unknown").shouldBe(true)
    ConfigRegistry.rejects("ui.element_gap", "wide").shouldBe(true)
    ConfigRegistry.rejects("ui.outline.thickness", "").shouldBe(true)
  }

  // issue #1046: command palette item spacing is now one of the metrics interface density itself governs, alongside
  // command surface min/max height and the overlay gap, rather than a separate flat-default knob.
  it should "scale command palette item gap rows with interface density" in {
    InterfaceDensityMetrics.forDensity(InterfaceDensity.Compact).itemGapRows shouldBe 0.0
    InterfaceDensityMetrics.forDensity(InterfaceDensity.Comfortable).itemGapRows shouldBe 0.0
    InterfaceDensityMetrics.forDensity(InterfaceDensity.Spacious).itemGapRows shouldBe 1.0
  }

  // issue #1046 (review follow-up): `command_runner.visible_rows`'s density default completes the same
  // unification -- these row counts were chosen to reproduce each density's own `commandSurfaceMaxHeight` exactly
  // through `SurfaceFrameLayout.frameHeightForItemRows` (the same formula an explicit override already used), so a
  // config that never set `command_runner.visible_rows` sees no change in the command palette's default height.
  it should "derive each density's default visible-row count from its own command surface metrics" in {
    def defaultHeightFor(density: InterfaceDensity): Int =
      val metrics = InterfaceDensityMetrics.forDensity(density)
      SurfaceFrameLayout.frameHeightForItemRows(
        metrics.visibleRows,
        hasHeader = true,
        hasFooter = true,
        borderCells = SurfaceFrameLayout.CommandSurfaceBorderCells,
        itemGapRows = metrics.itemGapRows,
        itemTargetRows = SurfaceFrameLayout.minimumTargetRows(density)
      )

    defaultHeightFor(InterfaceDensity.Compact).shouldBe(
      InterfaceDensityMetrics.forDensity(InterfaceDensity.Compact).commandSurfaceMaxHeight
    )
    defaultHeightFor(InterfaceDensity.Comfortable).shouldBe(
      InterfaceDensityMetrics.forDensity(InterfaceDensity.Comfortable).commandSurfaceMaxHeight
    )
    defaultHeightFor(InterfaceDensity.Spacious).shouldBe(
      InterfaceDensityMetrics.forDensity(InterfaceDensity.Spacious).commandSurfaceMaxHeight
    )
  }

  // issue #1542: outline thickness is configured in pixels, but the pixels a panel actually
  // occupies grow with the UI font size -- so the drawn chrome must scale by the same factor, rather than staying
  // fixed while everything around it grows (which is what made larger fonts look "proportionally tight").
  it should "draw the UI outline at its configured pixel value at the baseline font size" in {
    val config = AppConfig.default.withInterfaceConfig(
      InterfaceConfig(outlineThicknessPx = 2)
    )

    config.scaledUiOutlineThicknessPx shouldBe 2.0f
  }

  it should "scale UI outline thickness up as the UI font size grows" in {
    val config = AppConfig.default
      .withInterfaceConfig(InterfaceConfig(outlineThicknessPx = 2))
      .withFontConfig(AppConfig.default.editorConfig.fontConfig.copy(uiFontSize = 24.0f))

    config.uiChromeScale shouldBe 2.0
    config.scaledUiOutlineThicknessPx shouldBe 4.0f
  }

  it should "scale UI outline thickness down as UI font size shrinks, never to zero" in {
    val config = AppConfig.default
      .withInterfaceConfig(InterfaceConfig(outlineThicknessPx = 1))
      .withFontConfig(AppConfig.default.editorConfig.fontConfig.copy(uiFontSize = 6.0f))

    config.scaledUiOutlineThicknessPx should be > 0.0f
  }
