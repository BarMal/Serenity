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
