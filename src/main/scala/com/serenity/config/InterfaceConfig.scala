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
    commandSurfaceVerticalPadding: Int,
    // issue #1046: the command palette's per-item row spacing, unified onto the same one density control as
    // `commandSurfaceMaxHeight`/`overlayGapRows` rather than a separate `command_runner.item_gap_rows` knob that
    // defaulted to a flat 0.0 regardless of density.
    itemGapRows: Double
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
          commandSurfaceVerticalPadding = 2,
          itemGapRows = 0.0
        )
      case InterfaceDensity.Comfortable =>
        InterfaceDensityMetrics(
          gutterHeight = 1,
          overlayGapRows = 1,
          commandSurfaceMaxHeight = 10,
          commandSurfaceMinHeight = 6,
          commandSurfaceVerticalPadding = 3,
          itemGapRows = 0.0
        )
      case InterfaceDensity.Spacious =>
        InterfaceDensityMetrics(
          gutterHeight = 2,
          overlayGapRows = 2,
          commandSurfaceMaxHeight = 12,
          commandSurfaceMinHeight = 8,
          commandSurfaceVerticalPadding = 4,
          itemGapRows = 1.0
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
