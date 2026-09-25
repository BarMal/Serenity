package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.state.models.{AppState, SurfaceContent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** issue #1048: MRU tracking survives the palette closing and reopening within the same session, since `CommandRunner`
  * itself is reconstructed fresh on every open (`AppEventReducer.openCommandRunner`) -- the generation counter has to
  * live on `AppState.persisted` instead, and get seeded back into the freshly-activated runner.
  */
class CommandUsageMruSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def runnerFrom(sm: com.serenity.state.manager.StateManager): CommandRunner =
    sm.getCurrentState
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None)
      .getOrElse(fail("expected the command palette to be open"))

  "executing a command" should "record its use on AppState.persisted, surviving the palette closing" in {
    val sm = createStateManager("CommandUsageMru")

    val command = Command.typed(
      "test-mru-command",
      "A test command for MRU tracking",
      CommandIntent.Edit(EditIntent.Undo),
      label = "Test MRU Command"
    )
    sm.executeCommand(command).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().persisted.commandUsage should contain key command.name
  }

  it should "seed the freshly-activated palette's own commandUsage on the next open" in {
    val sm = createStateManager("CommandUsageMruReopen")
    val command = Command.typed(
      "test-mru-reopen-command",
      "A test command for MRU tracking across reopen",
      CommandIntent.Edit(EditIntent.Undo),
      label = "Test MRU Reopen Command"
    )
    sm.executeCommand(command).unsafeRunSync()

    // Open, then close, then reopen the palette -- CommandRunner.empty is reconstructed fresh each time.
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    runnerFrom(sm).commandUsage should contain key command.name
  }

  // #1714: a command that opens a surface commits it on top of the state `interpretCommand` left, so the MRU bump
  // recorded just before is not overwritten by the snapshot the command started from.
  private val earlierCommand =
    Command.typed(
      "test-mru-earlier-command",
      "An earlier command",
      CommandIntent.Edit(EditIntent.Undo),
      label = "Earlier"
    )

  private def runFromPalette(sm: com.serenity.state.manager.StateManager, label: String, commandName: String): Unit =
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    label.foreach(char => sm.applyEvent(InsertChar(char)).unsafeRunSync())
    runnerFrom(sm).selectedCommand.map(_.name) shouldBe Some(commandName)
    sm.applyEvent(Enter).unsafeRunSync()

  private def mostRecentCommand(state: AppState): Option[String] =
    state.persisted.commandUsage.maxByOption(_._2).map(_._1)

  "running a surface-opening command from the palette" should "leave the theme chooser open and most recent" in {
    val sm = createStateManager("CommandUsageMruThemeChooser")
    sm.executeCommand(earlierCommand).unsafeRunSync()

    runFromPalette(sm, "Open Theme Chooser", "theme-chooser")

    val state = sm.getCurrentState.unsafeRunSync()
    mostRecentCommand(state) shouldBe Some("theme-chooser")
    state.runtime.uiSurfaces.map(_.content).collect { case picker: SurfaceContent.ThemePicker => picker } should
      have size 1
  }

  it should "leave the theme creator open and most recent" in {
    val sm = createStateManager("CommandUsageMruThemeCreator")
    sm.executeCommand(earlierCommand).unsafeRunSync()

    runFromPalette(sm, "Open Theme Creator", "theme-creator")

    val state = sm.getCurrentState.unsafeRunSync()
    mostRecentCommand(state) shouldBe Some("theme-creator")
    state.runtime.uiSurfaces.map(_.content).collect { case creator: SurfaceContent.ThemeCreator => creator } should
      have size 1
  }

  it should "leave file search open and most recent" in {
    val sm = createStateManager("CommandUsageMruFileSearch")
    sm.executeCommand(earlierCommand).unsafeRunSync()

    runFromPalette(sm, "File Search", "file-search")

    val state = sm.getCurrentState.unsafeRunSync()
    mostRecentCommand(state) shouldBe Some("file-search")
    state.fileSearchSurface shouldBe defined
  }
