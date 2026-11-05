package com.serenity

import com.serenity.command.{CommandRegistry, CommandRunner, CommandSurfaceItem}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2577 P1c: the Settings tree opens on its own, and a settings hit from the command picker opens it drilled to that
  * setting -- the same place the palette's own drill-in lands today.
  */
class OpenSettingsAtSpec extends AnyFlatSpec with Matchers:

  given Balance                   = Balance.default
  given registry: CommandRegistry = CommandRegistry.default

  private def runnerIn(state: AppState): Option[CommandRunner] =
    state.commandRunnerSurface.map(_.content).collect { case SurfaceContent.CommandPalette(runner) => runner }

  /** What the palette does today for `query`: type it, highlight the hit for `groupId`/`itemId`, press Enter. */
  private def paletteDrillIn(query: String, groupId: String, itemId: Option[String]): CommandRunner =
    val searched = CommandRunner.empty
      .activate(registry, AppState.initial.persisted.config, context = AppState.initial.commandRunnerContext)
      .openSettings
      .updateSettingsSearch(query)
    val index = searched.visibleItems.indexWhere(isTarget(groupId, itemId))
    index should be >= 0
    searched.moveSelection(index).enterSelectedGroup

  private def isTarget(groupId: String, itemId: Option[String])(item: CommandSurfaceItem): Boolean =
    item match
      case setting: CommandSurfaceItem.SettingSearchItem =>
        setting.targetGroupId == groupId && itemId.contains(setting.targetItemId)
      case group: CommandSurfaceItem.GroupItem => group.id == groupId && itemId.isEmpty
      case _                                   => false

  "Opening settings at a setting" should "land where the palette's drill-in to that setting lands" in {
    val opened = CommandRunnerReducer.openSettingsAt(
      AppState.initial,
      registry,
      "settings-font-sizes",
      Some("font-sizes-code"),
      "font size"
    )
    val runner = runnerIn(opened).getOrElse(fail("settings did not open"))

    runner.isActive shouldBe true
    runner.activeSettingsSurface shouldBe paletteDrillIn(
      "font size",
      "settings-font-sizes",
      Some("font-sizes-code")
    ).activeSettingsSurface
    runner.activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-font-sizes")
    opened.persisted.focus shouldBe Focus.Surface(opened.commandRunnerSurface.map(_.id).getOrElse(fail("no surface")))
  }

  it should "open a whole group when no setting is named" in {
    val opened = CommandRunnerReducer.openSettingsAt(AppState.initial, registry, "settings-font-sizes", None, "font")

    runnerIn(opened).flatMap(_.activeSettingsSurface).map(_.current.groupId) shouldBe Some("settings-font-sizes")
  }

  it should "step back one level per Escape, through its parent group, to the settings root still showing the query" in {
    val opened =
      CommandRunnerReducer.openSettingsAt(
        AppState.initial,
        registry,
        "settings-font-sizes",
        Some("font-sizes-code"),
        "font size"
      )
    def escaped(state: AppState): CommandRunner =
      runnerIn(CommandRunnerReducer.reduce(RunnerDismiss, state, registry).state)
        .getOrElse(fail("runner closed before reaching the settings root"))

    val parent = CommandRunnerReducer.reduce(RunnerDismiss, opened, registry).state
    escaped(opened).activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-typography")

    val root = escaped(parent)
    root.isSettingsSurface shouldBe true
    root.activeSettingsSurface shouldBe None
    root.searchTerm shouldBe "font size"
  }

  it should "fall back to the settings root showing the query when the setting is not among its hits" in {
    val opened =
      CommandRunnerReducer.openSettingsAt(AppState.initial, registry, "no-such-group", Some("nothing"), "font size")
    val runner = runnerIn(opened).getOrElse(fail("settings did not open"))

    runner.isSettingsSurface shouldBe true
    runner.activeSettingsSurface shouldBe None
    runner.searchTerm shouldBe "font size"
  }

  "Opening settings" should "keep an already open settings surface open rather than toggling it shut" in {
    val once  = CommandRunnerReducer.openSettings(AppState.initial, registry)
    val twice = CommandRunnerReducer.openSettings(once, registry)

    runnerIn(twice).map(runner => runner.isActive && runner.isSettingsSurface) shouldBe Some(true)
  }
