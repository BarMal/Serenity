package com.serenity

import com.serenity.command.*
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.keystroke.events.RunnerSubmit
import com.serenity.project.ProjectPresence
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerReducer
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Project commands in a code workspace with no project around the working file stay in the palette, greyed out with
  * the reason, instead of disappearing: they are still the right commands for this mode, just not runnable yet.
  */
class CommandRunnerProjectPresenceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry        = CommandRegistry.default
  given CommandRegistry       = registry
  private val codeEditing     = EditingContext(AppMode.Code, None, Shell.Gui, Focus.EditorPane(PaneId(0)))
  private val noProjectReason = Some("No project detected.")

  private def runner(presence: ProjectPresence): CommandRunner =
    CommandRunner.empty.activate(
      registry,
      AppConfig.default,
      context = CommandRunnerContext(editingContext = Some(codeEditing), projectPresence = presence)
    )

  private def commandItem(runner: CommandRunner, name: String): CommandSurfaceItem.CommandItem =
    runner.visibleItems
      .collectFirst { case item: CommandSurfaceItem.CommandItem if item.command.name == name => item }
      .getOrElse(fail(s"missing $name"))

  private def paletteState(runner: CommandRunner): AppState =
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState(
      persisted = Persisted(layout = Layout.empty, buffers = Map.empty, focus = Focus.Surface(surface.id)),
      runtime = Runtime(uiSurfaces = List(surface))
    )

  private def runnerIn(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .map(_.content)
      .collect { case SurfaceContent.CommandPalette(found) => found }
      .getOrElse(fail("expected the command runner to stay open"))

  "the palette" should "keep project commands listed but disabled, with the reason, when no project is detected" in {
    val noProject = runner(ProjectPresence.NotDetected)

    commandItem(noProject, "project-build").disabledReason shouldBe noProjectReason
    commandItem(noProject, "project-cancel").disabledReason shouldBe None
    commandItem(noProject, "save").disabledReason shouldBe None
    commandItem(noProject.updateSearchTerm("project build"), "project-build").disabledReason shouldBe noProjectReason
  }

  it should "enable project commands once a project is detected" in {
    commandItem(runner(ProjectPresence.Detected), "project-build").disabledReason shouldBe None
  }

  it should "refuse a disabled command on Enter, keeping the palette open and saying why" in {
    val searched = runner(ProjectPresence.NotDetected).updateSearchTerm("project build")
    val selected = searched.withSelectedVisibleIndex(searched.visibleItems.indexWhere(_.id == "project-build"))

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, paletteState(selected), registry)

    submitted.effects shouldBe Nil
    runnerIn(submitted.state).statusMessage shouldBe noProjectReason
  }
