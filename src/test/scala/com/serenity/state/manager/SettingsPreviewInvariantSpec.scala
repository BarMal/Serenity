package com.serenity.state.manager

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{CommandRunnerReducer, ModalStateReducer}
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A previewed setting never outlives the command runner: whatever removes the runner surface, the commit that does it
  * puts the committed config back, so no path has to remember to.
  */
class SettingsPreviewInvariantSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry  = CommandRegistry.default
  private val previewed = AppConfig.default.withWordWrap(false)

  private def previewing: AppState =
    val opened = CommandRunner.empty.activate(registry, AppConfig.default).openSettings
    val index  = opened.submenuItems("settings-text-display").indexWhere(_.id == "line-wrap").max(0)
    val runner =
      opened.withDrilledSettingsSurface(SettingsSurfaceState(SettingsPage.Group("settings-text-display", index)))
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = AppState(
      persisted = Persisted(layout = Layout.empty, buffers = Map.empty, focus = Focus.Surface(surface.id)),
      runtime = Runtime(uiSurfaces = List(surface))
    )
    val cycled = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), state, registry).state
    cycled.copy(persisted = cycled.persisted.copy(config = previewed))

  private def withoutRunner(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = Nil)).popFocus

  private def prepared(next: AppState, before: AppState): AppState =
    StateManagerOperationBoundary.prepareCommit(next, before).fold(errors => fail(errors.mkString), identity)

  "Committing a state without the command runner" should "restore the committed config after a click outside" in {
    val before = previewing
    before.runtime.pendingSetting.isDefined shouldBe true

    val after = prepared(withoutRunner(before), before)

    after.runtime.pendingSetting shouldBe None
    after.persisted.config shouldBe AppConfig.default
  }

  it should "restore the committed config when another modal replaces the runner" in {
    val before   = previewing
    val picker   = ListPicker.loading("Pick")
    val replaced = ModalStateReducer.show(Modal.ListPicker(picker), withoutRunner(before)).state

    val after = prepared(replaced, before)

    after.runtime.pendingSetting shouldBe None
    after.persisted.config shouldBe AppConfig.default
  }

  it should "leave a preview alone while the runner is still open" in {
    val before = previewing

    val after = prepared(before, before)

    after.runtime.pendingSetting.isDefined shouldBe true
    after.persisted.config shouldBe previewed
  }
