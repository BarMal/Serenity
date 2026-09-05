package com.serenity

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterfaceConfigSpec extends AnyFlatSpec with Matchers:

  "InterfaceConfig" should "own interface density and chrome metric schema metadata" in {
    ConfigKeySchema.isKnownKey("interface.density") shouldBe true
    ConfigKeySchema.isKnownKey("ui.element_gap") shouldBe true
    ConfigKeySchema.isKnownKey("ui.element.gap") shouldBe true
    ConfigKeySchema.isKnownKey("ui.corner_radius") shouldBe true
    ConfigKeySchema.isKnownKey("ui.corner.radius") shouldBe true
    ConfigKeySchema.isKnownKey("ui.outline_thickness") shouldBe true
    ConfigKeySchema.isKnownKey("ui.outline.thickness") shouldBe true

    ConfigKeySchema.deprecatedKeys.should(
      contain allOf (
        "interface_density"    -> "interface.density",
        "ui_element_gap"       -> "ui.element_gap",
        "ui_corner_radius"     -> "ui.corner_radius",
        "ui_outline_thickness" -> "ui.outline_thickness"
      )
    )
  }

  it should "group interface density and chrome metrics under AppConfig" in {
    val config = AppConfig.default.withInterfaceConfig(
      InterfaceConfig(
        density = InterfaceDensity.Spacious,
        elementGap = 3,
        cornerRadiusPx = 12,
        outlineThicknessPx = 4
      )
    )

    config.interfaceConfig shouldBe InterfaceConfig(
      density = InterfaceDensity.Spacious,
      elementGap = 3,
      cornerRadiusPx = 12,
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
    val radiusConfig =
      ConfigRegistry.read(AppConfig.default, "ui_corner_radius", "14").getOrElse(fail("radius parse"))
    val outlineConfig =
      ConfigRegistry
        .read(AppConfig.default, "ui.outline.thickness", "5")
        .getOrElse(fail("outline parse"))

    densityConfig.interfaceConfig.density.shouldBe(InterfaceDensity.Spacious)
    gapConfig.interfaceConfig.elementGap.shouldBe(4)
    radiusConfig.interfaceConfig.cornerRadiusPx.shouldBe(14)
    outlineConfig.interfaceConfig.outlineThicknessPx.shouldBe(5)
    ConfigRegistry.read(AppConfig.default, "interface.density", "unknown").shouldBe(None)
  }

  it should "validate interface config entries centrally" in {
    ConfigRegistry.rejects("interface.density", "compact").shouldBe(false)
    ConfigRegistry.rejects("interface.density", "unknown").shouldBe(true)
    ConfigRegistry.rejects("ui.element_gap", "wide").shouldBe(true)
    ConfigRegistry.rejects("ui.corner_radius", "14").shouldBe(false)
    ConfigRegistry.rejects("ui.outline.thickness", "").shouldBe(true)
  }
