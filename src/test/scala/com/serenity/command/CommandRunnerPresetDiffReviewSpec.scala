package com.serenity.command

import com.serenity.config.UiLayout
import com.serenity.ui.presets.PresetChange
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `CommandRunner.openPresetDiffReview` and the `CommandRunnerSurface.PresetDiffReview` surface it opens: the preset
  * diff-toggle UI's own state, distinct from the generic `ToggleItem` primitive (`CommandRunnerReducerToggleSpec`) and
  * the diff computation itself (`UiPresetDiffSpec`).
  */
class CommandRunnerPresetDiffReviewSpec extends AnyFlatSpec with Matchers:

  private val changes = List(
    PresetChange(key = "editor.line_numbers", label = "Line Numbers", currentValue = "false", newValue = "true"),
    PresetChange(key = "theme", label = "Theme", currentValue = "Dark", newValue = "Light")
  )

  "openPresetDiffReview" should "switch to the review surface with one ToggleItem per change, all checked" in {
    val runner = CommandRunner.empty.openPresetDiffReview("Code", changes)

    runner.surface shouldBe a[CommandRunnerSurface.PresetDiffReview]
    val toggles = runner.visibleItems.collect { case t: CommandSurfaceItem.ToggleItem => t }
    toggles.map(_.id) shouldBe List("editor.line_numbers", "theme")
    toggles.map(runner.effectiveChecked) shouldBe List(true, true)
  }

  it should "append a trailing command that applies every change while none are toggled off" in {
    val runner = CommandRunner.empty.openPresetDiffReview("Code", changes)

    runner.visibleItems.last match
      case CommandSurfaceItem.CommandItem(command, _) =>
        command.intent shouldBe CommandIntent.UiPresets(
          UiPresetsIntent.ConfirmUiPresetDiffApply("Code", List("editor.line_numbers", "theme"))
        )
      case other => fail(s"expected a trailing CommandItem, got $other")
  }

  it should "bake only the still-checked changes into that command once one is toggled off" in {
    val opened  = CommandRunner.empty.openPresetDiffReview("Code", changes)
    val toggled = opened.copy(toggleSelections = Map("theme" -> false))

    toggled.visibleItems.last match
      case CommandSurfaceItem.CommandItem(command, _) =>
        command.intent shouldBe CommandIntent.UiPresets(
          UiPresetsIntent.ConfirmUiPresetDiffApply("Code", List("editor.line_numbers"))
        )
      case other => fail(s"expected a trailing CommandItem, got $other")
  }

  it should "reset any toggle selections left over from an earlier review" in {
    val stale = CommandRunner.empty.copy(toggleSelections = Map("theme" -> false))

    stale.openPresetDiffReview("Code", changes).toggleSelections shouldBe Map.empty
  }

  "a preset diff review" should "not count as a settings surface" in {
    val runner = CommandRunner.empty.openPresetDiffReview("Code", changes)

    runner.isSettingsSurface shouldBe false
    runner.activeSettingsSurface shouldBe None
  }

  it should "navigate across toggle rows and the trailing apply command, wrapping at the end" in {
    val runner = CommandRunner.empty.openPresetDiffReview("Code", changes)

    runner.selectedIndex shouldBe 0
    runner.moveSelection(1).selectedIndex shouldBe 1
    runner.moveSelection(1).moveSelection(1).selectedIndex shouldBe 2
    runner.moveSelection(1).moveSelection(1).moveSelection(1).selectedIndex shouldBe 0
  }

  it should "ignore typed search text rather than filtering or crashing" in {
    given CommandRegistry = CommandRegistry.default
    val runner            = CommandRunner.empty.openPresetDiffReview("Code", changes)

    val afterTyping = runner.updateSearchTerm("anything")

    afterTyping.surface shouldBe a[CommandRunnerSurface.PresetDiffReview]
    afterTyping.visibleItems.size shouldBe runner.visibleItems.size
  }

  it should "render an empty item list plus just the apply command when the preset changes nothing" in {
    val runner = CommandRunner.empty.openPresetDiffReview("Code", Nil)

    runner.visibleItems.collect { case t: CommandSurfaceItem.ToggleItem => t } shouldBe Nil
    runner.visibleItems should have size 1
  }

  private def layoutReview(selection: Map[String, Boolean] = Map.empty): CommandRunner =
    CommandRunner.empty
      .openPresetDiffReview("Minimal", changes.take(1), PresetDiffPurpose.SwitchLayout(UiLayout.Minimal))
      .copy(toggleSelections = selection)

  private def layoutConfirm(runner: CommandRunner): CommandIntent =
    runner.visibleItems.last match
      case CommandSurfaceItem.CommandItem(command, _) => command.intent
      case other                                      => fail(s"expected a trailing CommandItem, got $other")

  "a layout override review" should "apply the layout to every key left checked" in {
    layoutConfirm(layoutReview()) shouldBe CommandIntent.Settings(
      SettingsIntent.InterfaceChrome(
        InterfaceChromeIntent.ConfirmUiLayoutOverride(UiLayout.Minimal, List("editor.line_numbers"))
      )
    )
  }

  it should "keep the user's own value for a key toggled off" in {
    layoutConfirm(layoutReview(Map("editor.line_numbers" -> false))) shouldBe CommandIntent.Settings(
      SettingsIntent.InterfaceChrome(InterfaceChromeIntent.ConfirmUiLayoutOverride(UiLayout.Minimal, Nil))
    )
  }

  it should "still list one checked toggle per pinned key" in {
    val toggles = layoutReview().visibleItems.collect { case t: CommandSurfaceItem.ToggleItem => t }

    toggles.map(_.id) shouldBe List("editor.line_numbers")
    toggles.map(_.label) shouldBe List("Line Numbers: false -> true")
  }

  it should "say it is choosing between the layout and the user's settings, not applying a preset" in {
    layoutReview().surface match
      case review: CommandRunnerSurface.PresetDiffReview =>
        review.purpose.header(review.presetName) shouldBe "Use the Minimal layout (uncheck a setting to keep yours)"
      case other => fail(s"expected the review surface, got $other")
  }

  "a preset review" should "still head the list with the preset it applies" in {
    CommandRunner.empty.openPresetDiffReview("Code", changes).surface match
      case review: CommandRunnerSurface.PresetDiffReview =>
        review.purpose.header(review.presetName) shouldBe "Apply preset: Code"
      case other => fail(s"expected the review surface, got $other")
  }
