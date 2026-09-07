package com.serenity.config

enum InterfaceDensity:
  case Compact
  case Comfortable
  case Spacious

  def configKey: String =
    this match
      case Compact     => "compact"
      case Comfortable => "comfortable"
      case Spacious    => "spacious"

object InterfaceDensity:

  def fromConfigKey(value: String): Option[InterfaceDensity] =
    value.trim.toLowerCase match
      case "compact"     => Some(InterfaceDensity.Compact)
      case "comfortable" => Some(InterfaceDensity.Comfortable)
      case "spacious"    => Some(InterfaceDensity.Spacious)
      case _             => None

final case class InterfaceDensityMetrics(
    gutterHeight: Int,
    overlayGapRows: Int,
    commandSurfaceMaxHeight: Int,
    commandSurfaceMinHeight: Int,
    commandSurfaceVerticalPadding: Int
)

object InterfaceDensityMetrics:

  def forDensity(density: InterfaceDensity): InterfaceDensityMetrics =
    density match
      case InterfaceDensity.Compact =>
        InterfaceDensityMetrics(
          gutterHeight = 1,
          overlayGapRows = 0,
          commandSurfaceMaxHeight = 6,
          commandSurfaceMinHeight = 3,
          commandSurfaceVerticalPadding = 2
        )
      case InterfaceDensity.Comfortable =>
        InterfaceDensityMetrics(
          gutterHeight = 1,
          overlayGapRows = 1,
          commandSurfaceMaxHeight = 10,
          commandSurfaceMinHeight = 6,
          commandSurfaceVerticalPadding = 3
        )
      case InterfaceDensity.Spacious =>
        InterfaceDensityMetrics(
          gutterHeight = 2,
          overlayGapRows = 2,
          commandSurfaceMaxHeight = 12,
          commandSurfaceMinHeight = 8,
          commandSurfaceVerticalPadding = 4
        )

final case class InterfaceConfig(
    density: InterfaceDensity = InterfaceDensity.Comfortable,
    elementGap: Double = 0.0,
    cornerRadiusPx: Int = 8,
    outlineThicknessPx: Int = 2
):

  def normalized: InterfaceConfig =
    copy(
      elementGap = AppConfig.clampUiElementGap(elementGap),
      cornerRadiusPx = AppConfig.clampUiCornerRadiusPx(cornerRadiusPx),
      outlineThicknessPx = AppConfig.clampUiOutlineThicknessPx(outlineThicknessPx)
    )

final case class InputConfig(
    hotkeyConfig: HotkeyConfig = HotkeyConfig(),
    focusedKeymapConfig: FocusedKeymapConfig = FocusedKeymapConfig(),
    // Lines a single wheel notch scrolls. Three is the platform convention (`java.awt.event.MouseWheelEvent`'s own
    // unit-scroll default, and what most terminals send per notch), but it is a matter of taste and pointing device.
    wheelScrollLines: Int = 3
):

  def normalized: InputConfig =
    copy(wheelScrollLines = AppConfig.clampWheelScrollLines(wheelScrollLines))
