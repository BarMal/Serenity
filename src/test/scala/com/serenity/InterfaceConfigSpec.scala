package com.serenity

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterfaceConfigSpec extends AnyFlatSpec with Matchers:

  "InterfaceConfig" should "own interface density and chrome metric schema metadata" in {
    InterfaceConfig.Schema.currentKeys.should(
      contain allOf (
        "interface.density",
        "ui.element_gap",
        "ui.element.gap",
        "ui.corner_radius",
        "ui.corner.radius",
        "ui.outline_thickness",
        "ui.outline.thickness"
      )
    )

    InterfaceConfig.Schema.deprecatedKeys.should(
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
      InterfaceConfig.Schema
        .parse(AppConfig.default, "interface_density", "spacious")
        .getOrElse(fail("density parse"))
    val gapConfig =
      InterfaceConfig.Schema.parse(AppConfig.default, "ui.element_gap", "4").getOrElse(fail("gap parse"))
    val radiusConfig =
      InterfaceConfig.Schema.parse(AppConfig.default, "ui_corner_radius", "14").getOrElse(fail("radius parse"))
    val outlineConfig =
      InterfaceConfig.Schema
        .parse(AppConfig.default, "ui.outline.thickness", "5")
        .getOrElse(fail("outline parse"))

    densityConfig.interfaceConfig.density.shouldBe(InterfaceDensity.Spacious)
    gapConfig.interfaceConfig.elementGap.shouldBe(4)
    radiusConfig.interfaceConfig.cornerRadiusPx.shouldBe(14)
    outlineConfig.interfaceConfig.outlineThicknessPx.shouldBe(5)
    InterfaceConfig.Schema.parse(AppConfig.default, "interface.density", "unknown").shouldBe(None)
  }

  it should "preserve decimal element gaps and reject non-finite values" in {
    val decimal = InterfaceConfig.Schema
      .parse(AppConfig.default, "ui.element_gap", "0.25")
      .getOrElse(fail("decimal gap parse"))

    decimal.interfaceConfig.elementGap shouldBe 0.25
    InterfaceConfig.Schema.parse(AppConfig.default, "ui.element_gap", "NaN") shouldBe None
    InterfaceConfig.Schema.parse(AppConfig.default, "ui.element_gap", "Infinity") shouldBe None
  }

  it should "validate interface config entries centrally" in {
    InterfaceConfig.Schema.invalidValue("interface.density", "compact").shouldBe(false)
    InterfaceConfig.Schema.invalidValue("interface.density", "unknown").shouldBe(true)
    InterfaceConfig.Schema.invalidValue("ui.element_gap", "wide").shouldBe(true)
    InterfaceConfig.Schema.invalidValue("ui.corner_radius", "14").shouldBe(false)
    InterfaceConfig.Schema.invalidValue("ui.outline.thickness", "").shouldBe(true)
  }
