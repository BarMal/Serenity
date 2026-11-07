package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A named-session command run from the palette is recorded as the most recent command and opens its prompt: the prompt
  * is shown on the state current when it opens, not on the snapshot taken before the command was recorded.
  */
class SessionCommandUsageSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def createStateManager(): StateManager =
    createStateManager("SessionCommandUsageSpec", initialConfig = AppConfig.default)

  private def runFromPalette(stateManager: StateManager, commandName: String): AppState =
    (stateManager.applyEvent(ToggleCommandRunner) >>
      commandName.toList.traverseVoid(char => stateManager.applyEvent(InsertChar(char))) >>
      stateManager.applyEvent(Enter) >>
      stateManager.runtimeLifecycle.awaitEffects >>
      stateManager.getCurrentState).unsafeRunSync()

  private def mostRecentCommand(state: AppState): Option[String] =
    state.persisted.commandUsage.maxByOption(_._2).map(_._1.value)

  private def openModal(state: AppState): Option[Modal] =
    state.runtime.uiSurfaces.map(_.content).collectFirst { case SurfaceContent.ModalWorkflow(modal) => modal }

  "Save Session As from the palette" should "be the most recent command and show the name prompt" in {
    val after = runFromPalette(createStateManager(), "save-session-as")

    mostRecentCommand(after) shouldBe Some("save-session-as")
    openModal(after) should matchPattern {
      case Some(Modal.TextPrompt(TextPrompt(_, _, TextPromptPurpose.SessionName(SessionNamePromptMode.SaveAs)))) =>
    }
  }

  "Open Session from the palette" should "be the most recent command and show the session picker" in {
    val after = runFromPalette(createStateManager(), "open-session")

    mostRecentCommand(after) shouldBe Some("open-session")
    openModal(after) should matchPattern { case Some(Modal.ListPicker(picker)) if picker.title == "Open session" => }
  }

  "Rename Session from the palette" should "be the most recent command and show the session picker" in {
    val after = runFromPalette(createStateManager(), "rename-session")

    mostRecentCommand(after) shouldBe Some("rename-session")
    openModal(after) should matchPattern { case Some(Modal.ListPicker(picker)) if picker.title == "Rename session" => }
  }
end SessionCommandUsageSpec
