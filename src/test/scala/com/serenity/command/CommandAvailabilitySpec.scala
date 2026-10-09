package com.serenity.command

import com.serenity.config.{AppConfig, AppMode, HotkeyConfig}
import com.serenity.keystroke.events.RunCommand
import com.serenity.project.ProjectPresence
import com.serenity.rope.Balance
import com.serenity.state.models.{
  AppState,
  Buffer,
  BufferId,
  BufferKind,
  CursorPosition,
  EditingContext,
  Focus,
  PaneId,
  Selection,
  Shell
}
import com.serenity.state.reducers.{AppEffect, AppEventReducer}
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** One per-command availability (#1884) decides whether the palette offers a command, greys it out with a reason or
  * ranks it up, and whether a key bound to it runs it.
  */
class CommandAvailabilitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.default

  private def registered(id: String): Command =
    registry.findCommand(id).getOrElse(fail(s"$id is not registered"))

  private def editing(mode: AppMode, hasSelection: Boolean): EditingContext =
    EditingContext(mode, Some(BufferKind.PlainText), Shell.Gui, Focus.EditorPane(PaneId(0)), hasSelection)

  private def runnerContext(mode: AppMode, hasSelection: Boolean): CommandRunnerContext =
    CommandRunnerContext(editingContext = Some(editing(mode, hasSelection)))

  private def availabilityOf(id: String, context: CommandRunnerContext): Availability =
    CommandAvailability.of(registered(id), context)

  private def palette(context: CommandRunnerContext): CommandRunner =
    CommandRunner.empty.activate(registry, AppConfig.default, context = context)

  private def commandItems(runner: CommandRunner): List[CommandSurfaceItem.CommandItem] =
    runner.visibleItems.collect { case item: CommandSurfaceItem.CommandItem => item }

  private def searched(context: CommandRunnerContext, term: String): List[String] =
    commandItems(palette(context).updateSearchTerm(term)(using registry)).map(_.command.name)

  private val prose        = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux")).withAppMode(AppMode.Prose)
  private val proseWithout = runnerContext(AppMode.Prose, hasSelection = false)
  private val proseSelecting = runnerContext(AppMode.Prose, hasSelection = true)

  private def proseState(selection: Option[Selection]): AppState =
    val initial = AppState.initial(prose)
    val buffer = Buffer
      .fromString(BufferId(0), "alpha beta")
      .copy(editing = EditingStateFixtures(cursors = List(CursorPosition(0, 5)), selection = selection))
    initial.copy(persisted = initial.persisted.copy(buffers = Map(BufferId(0) -> buffer)))

  private val selectingAlpha = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5)))

  "A selection-only command" should "be disabled with a reason when there is no selection" in {
    availabilityOf("cut-to-darlings", proseWithout) shouldBe Availability.Disabled(CommandAvailability.NeedsSelection)
  }

  it should "be boosted when there is a selection" in {
    availabilityOf("cut-to-darlings", proseSelecting) shouldBe Availability.Boosted
  }

  "A command that acts on the selection or the line" should "stay enabled without one and be boosted with one" in {
    availabilityOf("copy", proseWithout) shouldBe Availability.Enabled
    availabilityOf("copy", proseSelecting) shouldBe Availability.Boosted
    availabilityOf("bold", proseWithout) shouldBe Availability.Enabled
    availabilityOf("bold", proseSelecting) shouldBe Availability.Boosted
  }

  "A command outside the mode" should "be hidden, selection or not" in {
    availabilityOf("cut-to-darlings", runnerContext(AppMode.Code, hasSelection = true)) shouldBe Availability.Hidden
  }

  "A command whose prerequisite is unmet" should "be disabled with the prerequisite's reason" in {
    val noProject =
      CommandRunnerContext(
        editingContext = Some(editing(AppMode.Code, hasSelection = true)),
        projectPresence = ProjectPresence.NotDetected
      )

    availabilityOf("project-build", noProject) shouldBe Availability.Disabled(CommandPrerequisites.NoProjectDetected)
  }

  "Any command" should "be enabled when no editing context is known" in {
    availabilityOf("cut-to-darlings", CommandRunnerContext.empty) shouldBe Availability.Enabled
  }

  "The palette" should "grey a selection-only command out with the reason when there is no selection" in {
    def reasonFor(context: CommandRunnerContext): Option[Option[String]] =
      commandItems(palette(context).updateSearchTerm("cut to darlings")(using registry))
        .find(_.command.name == "cut-to-darlings")
        .map(_.disabledReason)

    reasonFor(proseWithout) shouldBe Some(Some(CommandAvailability.NeedsSelection))
    reasonFor(proseSelecting) shouldBe Some(None)
  }

  it should "rank a boosted command above an equally matching one only when there is a selection" in {
    def closeBeforeCutToDarlings(context: CommandRunnerContext): Boolean =
      val names = searched(context, "c")
      names.indexOf("close") < names.indexOf("cut-to-darlings")

    closeBeforeCutToDarlings(proseWithout) shouldBe true
    closeBeforeCutToDarlings(proseSelecting) shouldBe false
  }

  it should "open with the commands boosted by the selection first, after Settings" in {
    val opening = commandItems(palette(proseSelecting).recordCommandUsage("save")).map(_.command.name)

    opening.headOption shouldBe Some("open-settings")
    opening.slice(1, 4) should contain allOf ("copy", "cut", "cut-to-darlings")
  }

  "A key bound to a selection-only command" should "run it only when there is a selection" in {
    def effects(state: AppState): List[AppEffect] =
      AppEventReducer.reduce(RunCommand("cut-to-darlings"), state, registry).effects

    effects(proseState(selection = None)) shouldBe Nil
    effects(proseState(selectingAlpha)) shouldBe List(AppEffect.ExecuteCommand(registered("cut-to-darlings")))
  }

  "The editing context" should "know whether the active buffer has a selection" in {
    proseState(selection = None).editingContext.hasSelection shouldBe false
    proseState(selectingAlpha).editingContext.hasSelection shouldBe true
  }
