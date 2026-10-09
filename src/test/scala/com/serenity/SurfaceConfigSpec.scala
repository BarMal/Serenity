package com.serenity

import com.serenity.config.*
import com.serenity.config.AppConfigOps.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceConfigSpec extends AnyFlatSpec with Matchers:

  // #1529: the diagnostic-highlight blend weight was a hardcoded literal in `RendererHighlights`; it's a config value
  // now, clamped to [0, 1] like the other alpha-shaped settings.
  "SurfaceConfig.normalized" should "clamp diagnosticHighlightBlendWeight to [0, 1]" in {
    SurfaceConfig(diagnosticHighlightBlendWeight = 5.0).normalized.diagnosticHighlightBlendWeight shouldBe 1.0
    SurfaceConfig(diagnosticHighlightBlendWeight = -5.0).normalized.diagnosticHighlightBlendWeight shouldBe 0.0
    SurfaceConfig(diagnosticHighlightBlendWeight = 0.6).normalized.diagnosticHighlightBlendWeight shouldBe 0.6
  }

  "AppConfig.default" should "default the diagnostic highlight blend weight to 0.45" in {
    AppConfig.default.surfaceConfig.diagnosticHighlightBlendWeight shouldBe 0.45
  }

  "AppConfig.withDiagnosticHighlightBlendWeight" should "change only the blend weight" in {
    val updated = AppConfig.default.withDiagnosticHighlightBlendWeight(0.75)

    updated.surfaceConfig.diagnosticHighlightBlendWeight shouldBe 0.75
  }

  "ConfigRegistry" should "read and validate the diagnostic highlight blend weight setting" in {
    ConfigRegistry
      .read(AppConfig.default, "ui.diagnostic_highlight_blend_weight", "0.2")
      .getOrElse(fail("ui.diagnostic_highlight_blend_weight parse"))
      .surfaceConfig
      .diagnosticHighlightBlendWeight shouldBe 0.2

    ConfigRegistry.rejects("ui.diagnostic_highlight_blend_weight", "0.2") shouldBe false
    ConfigRegistry.rejects("ui.diagnostic_highlight_blend_weight", "not-a-number") shouldBe true
  }

  "ConfigKeySchema" should "know the diagnostic highlight blend weight key" in {
    ConfigKeySchema.isKnownKey("ui.diagnostic_highlight_blend_weight") shouldBe true
  }

  // Column-based document layout (issue #1338, Phase 1): a global toggle -- no per-document/per-pane settings
  // infrastructure exists today -- that only takes effect while word wrap is also on.
  "SurfaceConfig" should "default column mode off, with a sensible target width and gap" in {
    val default = SurfaceConfig()
    default.columnModeEnabled.shouldBe(false)
    default.columnTargetWidthCells.shouldBe(80)
    default.columnGap.shouldBe(2)
  }

  // Multi-column e-reader layout (issue #1338, Phase 2 / slice 4): count-driven, Auto-inclusive. `columnCount = None`
  // means Auto (the width-driven "as many as fit" behaviour); `Some(n)` pins exactly n columns. The clamp to the
  // pane-dependent maximum lives in the resolver (`LayoutEngine.resolvedColumnCount`), not here, since `normalized`
  // has no pane width to clamp against.
  "SurfaceConfig" should "default columnCount to None (Auto)" in {
    SurfaceConfig().columnCount shouldBe None
  }

  "AppConfig.withColumnCount" should "set an explicit column count" in {
    AppConfig.default.withColumnCount(Some(3)).surfaceConfig.columnCount shouldBe Some(3)
  }

  it should "clear the column count back to Auto" in {
    AppConfig.default.withColumnCount(Some(3)).withColumnCount(None).surfaceConfig.columnCount shouldBe None
  }

  it should "leave columnCount untouched in normalized (its max is pane-width dependent, clamped in the resolver)" in {
    SurfaceConfig(columnCount = Some(999)).normalized.columnCount shouldBe Some(999)
    SurfaceConfig(columnCount = None).normalized.columnCount shouldBe None
  }

  "SurfaceConfig.normalized" should "clamp columnGap to non-negative" in {
    SurfaceConfig(columnGap = -5).normalized.columnGap.shouldBe(0)
    SurfaceConfig(columnGap = 5).normalized.columnGap.shouldBe(5)
  }

  it should "clamp columnTargetWidthCells to at least 1" in {
    SurfaceConfig(columnTargetWidthCells = -5).normalized.columnTargetWidthCells.shouldBe(1)
    SurfaceConfig(columnTargetWidthCells = 0).normalized.columnTargetWidthCells.shouldBe(1)
    SurfaceConfig(columnTargetWidthCells = 100).normalized.columnTargetWidthCells.shouldBe(100)
  }

  "SurfaceConfig.normalized" should "clamp rendererFrameStateCacheCapacity to AppConfig's configured bounds" in {
    SurfaceConfig(rendererFrameStateCacheCapacity = Int.MaxValue).normalized.rendererFrameStateCacheCapacity shouldBe
      AppConfig.MaxRendererFrameStateCacheCapacity
    SurfaceConfig(rendererFrameStateCacheCapacity = -100).normalized.rendererFrameStateCacheCapacity shouldBe
      AppConfig.MinRendererFrameStateCacheCapacity
    SurfaceConfig(rendererFrameStateCacheCapacity = 128).normalized.rendererFrameStateCacheCapacity shouldBe 128
  }

  // issue #1046: `commandRunnerItemGapRows` is now `Option[Double]`, falling back to interface density's own
  // default (via `AppConfig.effectiveCommandRunnerItemGapRows`) rather than a flat 0.0 -- an explicit override still
  // clamps to the configured bounds exactly as `commandRunnerCursorGapRows` already does, and `None` stays `None`.
  it should "clamp an explicit commandRunnerItemGapRows override and leave an absent one as None" in {
    SurfaceConfig(commandRunnerItemGapRows = Some(100.0)).normalized.commandRunnerItemGapRows shouldBe
      Some(AppConfig.MaxCommandRunnerItemGapRows)
    SurfaceConfig(commandRunnerItemGapRows = Some(-5.0)).normalized.commandRunnerItemGapRows shouldBe
      Some(AppConfig.MinCommandRunnerItemGapRows)
    SurfaceConfig(commandRunnerItemGapRows = None).normalized.commandRunnerItemGapRows shouldBe None
  }

  it should "fall back to interface density's item gap rows when no override is configured" in {
    val config = AppConfig.default.withInterfaceDensity(InterfaceDensity.Spacious)

    config.surfaceConfig.commandRunnerItemGapRows.shouldBe(None)
    config.effectiveCommandRunnerItemGapRows.shouldBe(
      InterfaceDensityMetrics.forDensity(InterfaceDensity.Spacious).itemGapRows
    )

    val overridden = config.withCommandRunnerItemGapRows(Some(4.0))
    overridden.effectiveCommandRunnerItemGapRows shouldBe 4.0
  }

  // issue #1046 (review follow-up): `commandRunnerVisibleRows`/`commandRunnerCursorGapRows` complete the same
  // density unification `effectiveCommandRunnerItemGapRows` already covers above.
  it should "fall back to interface density's visible row count when no override is configured" in {
    val config = AppConfig.default.withInterfaceDensity(InterfaceDensity.Spacious)

    config.surfaceConfig.commandRunnerVisibleRows.shouldBe(None)
    config.effectiveCommandRunnerVisibleRows.shouldBe(
      InterfaceDensityMetrics.forDensity(InterfaceDensity.Spacious).visibleRows
    )

    val overridden = config.withCommandRunnerVisibleRows(Some(7))
    overridden.effectiveCommandRunnerVisibleRows shouldBe 7
  }

  it should "fall back to interface density's overlay gap for the command palette's cursor gap when no override or UI element gap is configured" in {
    val config = AppConfig.default.withInterfaceDensity(InterfaceDensity.Spacious)

    config.surfaceConfig.commandRunnerCursorGapRows.shouldBe(None)
    config.effectiveCommandRunnerCursorGapRows.shouldBe(
      InterfaceDensityMetrics.forDensity(InterfaceDensity.Spacious).overlayGapRows.toDouble
    )

    val overridden = config.withCommandRunnerCursorGapRows(Some(4.0))
    overridden.effectiveCommandRunnerCursorGapRows shouldBe 4.0
  }

  it should "prefer an explicit UI element gap over interface density's overlay gap for the command palette's cursor gap" in {
    val config = AppConfig.default
      .withInterfaceDensity(InterfaceDensity.Compact)
      .withUiElementGap(Some(3.5))

    config.surfaceConfig.commandRunnerCursorGapRows shouldBe None
    config.effectiveCommandRunnerCursorGapRows shouldBe 3.5
  }

  "SurfaceConfig" should "leave every surface setting's key to ConfigRegistry" in {
    ConfigRegistry.allKeys.should(contain("editor.contextual_toolbar_mode"))
    ConfigRegistry.allKeys.should(contain("window.viewport.height_max"))
  }

  it should "group appearance and text display settings under AppConfig" in {
    val config = AppConfig.default
      .withTextAreaInsets(TextAreaInsets.fromPercent(20.0, 10.0))
      .withViewportSizing(
        ViewportSizing(
          width = ViewportAxisSizing.fromPercent(80.0, Some(120)),
          height = ViewportAxisSizing.fromPercent(90.0, Some(40))
        )
      )
      .withWordWrap(false)
      .withFocusedTextBody(true)
      .withContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly)

    config.surfaceConfig.textAreaInsets.shouldBe(TextAreaInsets.fromPercent(20.0, 10.0))
    config.surfaceConfig.viewportSizing.shouldBe(
      ViewportSizing(
        width = ViewportAxisSizing.fromPercent(80.0, Some(120)),
        height = ViewportAxisSizing.fromPercent(90.0, Some(40))
      )
    )
    config.surfaceConfig.wordWrapEnabled.shouldBe(false)
    config.surfaceConfig.focusedTextBodyEnabled.shouldBe(true)
    config.surfaceConfig.contextualToolbarDisplayMode.shouldBe(ToolbarDisplayMode.TextOnly)
  }

  it should "default the command runner's persistent key-hint footer to on, and expose a with-helper to toggle it" in {
    AppConfig.default.surfaceConfig.commandRunnerShowKeyHints.shouldBe(true)

    val disabled = AppConfig.default.withCommandRunnerShowKeyHints(false)
    disabled.surfaceConfig.commandRunnerShowKeyHints.shouldBe(false)

    disabled.withCommandRunnerShowKeyHints(true).surfaceConfig.commandRunnerShowKeyHints.shouldBe(true)
  }

  it should "default the contextual toolbar display mode to icons and text" in
    AppConfig.default.surfaceConfig.contextualToolbarDisplayMode.shouldBe(ToolbarDisplayMode.IconAndText)

  it should "leave interface settings owned by InterfaceConfig" in {
    val config = AppConfig.default
      .withInterfaceDensity(InterfaceDensity.Spacious)
      .withUiElementGap(Some(3))
      .withUiOutlineThicknessPx(4)

    config.interfaceConfig.shouldBe(
      InterfaceConfig(
        density = InterfaceDensity.Spacious,
        elementGap = Some(3),
        outlineThicknessPx = 4
      )
    )
  }
