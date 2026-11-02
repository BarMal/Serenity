package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.command.{
  Command,
  CommandCategory,
  CommandIntent,
  CommandRegistry,
  CommandRunner,
  CommandSurfaceItem,
  FontIntent,
  SettingsIntent
}
import com.serenity.config.{ConfigManager, ConfigVersion}
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.{ConfirmAction, ConfirmPrompt, Modal, SurfaceContent}
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.fonts.FontSizes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2405: Set All Sizes asks first, applying it sets the three sizes together, Undo Last Size Change restores the three
  * from before, and what undo goes back to survives a restart.
  */
class StateManagerFontSizesSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def editorOn(configPath: Path): StateManager =
    val initial = ConfigManager.loadConfigIO(Some(configPath.toString)).unsafeRunSync()
    StateManager
      .apply(
        testLogger("StateManagerFontSizesSpec"),
        sessionRootOverride = Some(TestTemp.directory("font-sizes-session")),
        initialConfig = initial,
        configPersistencePath = Some(configPath),
        dictionaryCache = SharedDictionary.cacheFor(initial)
      )
      .unsafeRunSync()

  private def configFile(text: String): Path =
    val path = TestTemp.directory("font-sizes-config").resolve("config.conf")
    Files.writeString(path, s"config.version = ${ConfigVersion.Current.value}\n$text")
    path

  private val startingFile =
    "typography.prose.size = 36.0\ntypography.code.size = 12.0\ntypography.ui.size = 11.0\n"

  private def sizesOf(stateManager: StateManager): FontSizes =
    stateManager.getCurrentState.unsafeRunSync().persisted.config.editorConfig.fontConfig.sizes

  private def previousOf(stateManager: StateManager): Option[FontSizes] =
    stateManager.getCurrentState.unsafeRunSync().persisted.config.editorConfig.fontConfig.previousSizes

  private def fontCommand(intent: FontIntent): Command =
    Command.typed(
      "font-sizes-test",
      "Change a font size",
      CommandIntent.Settings(SettingsIntent.Font(intent)),
      CommandCategory.Settings
    )

  private def run(stateManager: StateManager, intent: FontIntent): Unit =
    stateManager.executeCommand(fontCommand(intent)).unsafeRunSync()
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()

  private def prompts(stateManager: StateManager): List[ConfirmPrompt] =
    val state = stateManager.getCurrentState.unsafeRunSync()
    state.runtime.modalStack.map(_.modal).collect { case Modal.Confirm(prompt) => prompt } ++
      state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.Confirm(p)) => p }

  private def choose(stateManager: StateManager, label: String): Unit =
    prompts(stateManager).flatMap(_.choices.items).find(_.label == label).map(_.action) match
      case Some(ConfirmAction.Run(command)) =>
        stateManager.executeCommand(command).unsafeRunSync()
        stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()
      case other => fail(s"expected a runnable choice named $label, got $other")

  "The Font Sizes group" should "mark no size as the one in use, whatever the active document is" in {
    val stateManager      = editorOn(configFile(startingFile))
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val state             = stateManager.getCurrentState.unsafeRunSync()

    val runner = CommandRunner.empty
      .activate(registry, state.persisted.config, context = state.commandRunnerContext)
      .openSettings

    val labels = runner.settingsGroups
      .find(_.id == "settings-typography")
      .flatMap(_.children.collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-font-sizes" => group
      })
      .map(_.children.collect { case item: CommandSurfaceItem.InputItem => item.label })
      .getOrElse(fail("missing the Font Sizes group"))
    labels.take(3) shouldBe List("Prose Font Size", "Code Font Size", "UI Font Size")
  }

  "Asking to set all sizes" should "open a prompt showing each size before and after, and change nothing yet" in {
    val stateManager = editorOn(configFile(startingFile))

    run(stateManager, FontIntent.ConfirmSetAllFontSizes(16.0f))

    val prompt = prompts(stateManager) match
      case List(only) => only
      case other      => fail(s"expected one prompt, got $other")
    prompt.title shouldBe "Set All Font Sizes"
    prompt.message should contain("Prose: 36.0 -> 16.0")
    prompt.message should contain("Code: 12.0 -> 16.0")
    prompt.message should contain("UI: 11.0 -> 16.0")
    prompt.message.mkString(" ") should include("Undo Last Size Change")
    prompt.choices.items.map(_.label) shouldBe List("Apply", "Cancel")
    prompt.onDismiss shouldBe ConfirmAction.Dismiss
    prompt.choices.items.find(_.label == "Cancel").map(_.action) shouldBe Some(ConfirmAction.Dismiss)
    sizesOf(stateManager) shouldBe FontSizes(36.0f, 12.0f, 11.0f)
    previousOf(stateManager) shouldBe None
  }

  "Applying Set All Sizes" should "set the three together and keep the sizes it replaced" in {
    val stateManager = editorOn(configFile(startingFile))
    run(stateManager, FontIntent.ConfirmSetAllFontSizes(16.0f))

    choose(stateManager, "Apply")

    sizesOf(stateManager) shouldBe FontSizes(16.0f, 16.0f, 16.0f)
    previousOf(stateManager) shouldBe Some(FontSizes(36.0f, 12.0f, 11.0f))
  }

  "Undo Last Size Change" should "restore the three sizes from before the last change, once" in {
    val stateManager = editorOn(configFile(startingFile))
    run(stateManager, FontIntent.SetAllFontSizes(16.0f))

    run(stateManager, FontIntent.UndoFontSizeChange)

    sizesOf(stateManager) shouldBe FontSizes(36.0f, 12.0f, 11.0f)
    previousOf(stateManager) shouldBe None
    run(stateManager, FontIntent.UndoFontSizeChange)
    sizesOf(stateManager) shouldBe FontSizes(36.0f, 12.0f, 11.0f)
  }

  "A size change" should "be written with what undo goes back to, and read back after a restart" in {
    val path         = configFile(startingFile)
    val stateManager = editorOn(path)
    run(stateManager, FontIntent.SetTextFontSize(20.0f))

    Files.readString(path) should include("typography.previous_sizes")

    val restarted = editorOn(path)
    sizesOf(restarted) shouldBe FontSizes(20.0f, 12.0f, 11.0f)
    previousOf(restarted) shouldBe Some(FontSizes(36.0f, 12.0f, 11.0f))
    run(restarted, FontIntent.UndoFontSizeChange)
    sizesOf(restarted) shouldBe FontSizes(36.0f, 12.0f, 11.0f)
  }
