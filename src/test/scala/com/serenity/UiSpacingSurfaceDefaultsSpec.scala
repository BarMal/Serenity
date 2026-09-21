package com.serenity

import com.serenity.config.*
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pins the surface-aware defaults for the three spacing settings that used to be flush-to-edge on both surfaces
  * (`ui.element_gap`, `editor.line_number_margin_left`, `editor.line_number_padding`): unset now resolves to a GUI cell
  * of breathing room but leaves the TUI's existing density untouched, while an explicit value -- including an explicit
  * zero -- is honoured on both surfaces unchanged. See `AppState.effectiveUiElementGap` and its
  * `effectiveLineNumberMarginLeft`/`effectiveLineNumberPadding` siblings.
  */
class UiSpacingSurfaceDefaultsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val gui = AppState.initial
  private val tui = AppState.initial.copy(runtime = AppState.initial.runtime.copy(isTuiMode = true))

  "an unset UI element gap" should "default to one cell on the GUI and zero on the TUI" in {
    gui.persisted.config.uiElementGap shouldBe None
    gui.effectiveUiElementGap shouldBe 1.0

    tui.persisted.config.uiElementGap shouldBe None
    tui.effectiveUiElementGap shouldBe 0.0
  }

  "an explicit UI element gap" should "be honoured on both surfaces, including zero" in {
    val guiExplicit =
      gui.copy(persisted = gui.persisted.copy(config = gui.persisted.config.withUiElementGap(Some(3.0))))
    val tuiExplicit =
      tui.copy(persisted = tui.persisted.copy(config = tui.persisted.config.withUiElementGap(Some(3.0))))
    guiExplicit.effectiveUiElementGap shouldBe 3.0
    tuiExplicit.effectiveUiElementGap shouldBe 3.0

    val guiZero = gui.copy(persisted = gui.persisted.copy(config = gui.persisted.config.withUiElementGap(Some(0.0))))
    val tuiZero = tui.copy(persisted = tui.persisted.copy(config = tui.persisted.config.withUiElementGap(Some(0.0))))
    guiZero.effectiveUiElementGap shouldBe 0.0
    tuiZero.effectiveUiElementGap shouldBe 0.0
  }

  "an unset line-number margin/padding" should "default to one cell on the GUI and zero on the TUI" in {
    gui.effectiveLineNumberMarginLeft shouldBe 1
    gui.effectiveLineNumberPadding shouldBe 1
    tui.effectiveLineNumberMarginLeft shouldBe 0
    tui.effectiveLineNumberPadding shouldBe 0
  }

  "an explicit line-number margin/padding" should "be honoured on both surfaces, including zero" in {
    def withLayout(state: AppState, layout: LineNumberLayout): AppState =
      state.copy(persisted = state.persisted.copy(config = state.persisted.config.withLineNumberLayout(layout)))

    val explicit = LineNumberLayout(marginLeft = Some(4), padding = Some(2))
    withLayout(gui, explicit).effectiveLineNumberMarginLeft shouldBe 4
    withLayout(gui, explicit).effectiveLineNumberPadding shouldBe 2
    withLayout(tui, explicit).effectiveLineNumberMarginLeft shouldBe 4
    withLayout(tui, explicit).effectiveLineNumberPadding shouldBe 2

    val explicitZero = LineNumberLayout(marginLeft = Some(0), padding = Some(0))
    withLayout(gui, explicitZero).effectiveLineNumberMarginLeft shouldBe 0
    withLayout(gui, explicitZero).effectiveLineNumberPadding shouldBe 0
    withLayout(tui, explicitZero).effectiveLineNumberMarginLeft shouldBe 0
    withLayout(tui, explicitZero).effectiveLineNumberPadding shouldBe 0
  }

  "a config file that wrote an explicit zero" should "keep meaning zero on both surfaces, not fall back to unset" in {
    // Back-compat: `ui.element_gap = 0.0` on disk decodes to `Some(0.0)`, not `None` -- see
    // `ConfigRoundTripSpec`/`FieldCodec.orAuto` for the codec this relies on ("auto"/absent means unset, a real
    // number -- including zero -- means explicit).
    val zeroed = AppConfig.default.withUiElementGap(Some(0.0))
    zeroed.uiElementGap shouldBe Some(0.0)
    gui.copy(persisted = gui.persisted.copy(config = zeroed)).effectiveUiElementGap shouldBe 0.0
    tui.copy(persisted = tui.persisted.copy(config = zeroed)).effectiveUiElementGap shouldBe 0.0
  }

  // `AppState.effectiveCommandRunnerCursorGapRows` mirrors `FloatingSurfaceLayout.floatingStackGapRows`'s fallback
  // chain -- the command palette's cursor gap should track the surface-aware UI element gap the same way every other
  // floating surface's stack gap already does, rather than the AppConfig-only accessor's surface-blind read of the
  // raw `uiElementGap` field (issue #1621 carve-out).
  "an unset command-runner cursor gap" should "fall back to the surface-aware UI element gap when it is positive" in {
    gui.persisted.config.surfaceConfig.commandRunnerCursorGapRows shouldBe None
    gui.effectiveCommandRunnerCursorGapRows shouldBe gui.effectiveUiElementGap
    gui.effectiveCommandRunnerCursorGapRows shouldBe 1.0
  }

  it should "fall back to interface density's overlay gap on the TUI, where the UI element gap defaults to zero" in {
    tui.persisted.config.surfaceConfig.commandRunnerCursorGapRows shouldBe None
    tui.effectiveUiElementGap shouldBe 0.0
    tui.effectiveCommandRunnerCursorGapRows shouldBe
      InterfaceDensityMetrics.forDensity(tui.persisted.config.interfaceDensity).overlayGapRows.toDouble
  }

  it should "prefer an explicit UI element gap over interface density's overlay gap, on either surface" in {
    val guiExplicit =
      gui.copy(persisted = gui.persisted.copy(config = gui.persisted.config.withUiElementGap(Some(3.5))))
    val tuiExplicit =
      tui.copy(persisted = tui.persisted.copy(config = tui.persisted.config.withUiElementGap(Some(3.5))))
    guiExplicit.effectiveCommandRunnerCursorGapRows shouldBe 3.5
    tuiExplicit.effectiveCommandRunnerCursorGapRows shouldBe 3.5
  }

  "an explicit command-runner cursor gap override" should "win regardless of surface or UI element gap" in {
    def withOverride(state: AppState): AppState =
      state.copy(persisted =
        state.persisted.copy(config = state.persisted.config.withCommandRunnerCursorGapRows(Some(4.0)))
      )
    withOverride(gui).effectiveCommandRunnerCursorGapRows shouldBe 4.0
    withOverride(tui).effectiveCommandRunnerCursorGapRows shouldBe 4.0
  }
