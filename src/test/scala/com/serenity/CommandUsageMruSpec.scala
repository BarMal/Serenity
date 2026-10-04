package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.keystroke.events.{Enter, InsertChar, MoveDown, ToggleCommandRunner}
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.{AppState, ListPicker, Modal, SurfaceContent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** issue #1048: MRU tracking survives the palette closing and reopening within the same session, since `CommandRunner`
  * itself is reconstructed fresh on every open (`AppEventReducer.openCommandRunner`) -- the generation counter has to
  * live on `AppState.persisted` instead, and get seeded back into the freshly-activated runner.
  */
class CommandUsageMruSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def registryCommand(name: String): Command =
    CommandRegistry.withToggleUI.findCommand(name).getOrElse(fail(s"no registry command $name"))

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

    val command = registryCommand("undo")
    sm.executeCommand(command).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().persisted.commandUsage should contain key CommandId(command.name)
  }

  it should "seed the freshly-activated palette's own commandUsage on the next open" in {
    val sm      = createStateManager("CommandUsageMruReopen")
    val command = registryCommand("undo")
    sm.executeCommand(command).unsafeRunSync()

    // Open, then close, then reopen the palette -- CommandRunner.empty is reconstructed fresh each time.
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    runnerFrom(sm).commandUsage should contain key CommandId(command.name)
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
    state.persisted.commandUsage.maxByOption(_._2).map(_._1.value)

  private def themeChooser(state: AppState): Option[ListPicker] =
    state.runtime.uiSurfaces.map(_.content).collectFirst {
      case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) if picker.title == "Theme" => picker
    }

  "running a surface-opening command from the palette" should "leave the theme chooser open and most recent" in {
    val sm = createStateManager("CommandUsageMruThemeChooser")
    sm.executeCommand(earlierCommand).unsafeRunSync()

    runFromPalette(sm, "Open Theme Chooser", "theme-chooser")

    val state = sm.getCurrentState.unsafeRunSync()
    mostRecentCommand(state) shouldBe Some("theme-chooser")
    themeChooser(state) shouldBe defined
  }

  // #1877: a theme pick is no palette command, so it never enters the recency table.
  "the theme chooser" should "leave command usage untouched while previewing and by the theme picked" in {
    val sm = createStateManager("CommandUsageMruThemePreview")
    runFromPalette(sm, "Open Theme Chooser", "theme-chooser")

    sm.applyEvent(MoveDown).unsafeRunSync()
    val previewing = sm.getCurrentState.unsafeRunSync()
    mostRecentCommand(previewing) shouldBe Some("theme-chooser")

    themeChooser(previewing).flatMap(_.selectedChoice) shouldBe defined
    sm.applyEvent(Enter).unsafeRunSync()
    val picked = sm.getCurrentState.unsafeRunSync()
    themeChooser(picked) shouldBe None
    mostRecentCommand(picked) shouldBe Some("theme-chooser")
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

    runFromPalette(sm, "Search in Open Files", "file-search")

    val state = sm.getCurrentState.unsafeRunSync()
    mostRecentCommand(state) shouldBe Some("file-search")
    state.runtime.uiSurfaces.map(_.content).collect {
      case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker.title
    } shouldBe List("Search in Open Files")
  }
