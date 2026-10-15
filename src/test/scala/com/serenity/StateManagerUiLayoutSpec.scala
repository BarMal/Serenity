package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.command.{
  Command,
  CommandCategory,
  CommandIntent,
  CommandRunnerSurface,
  CommandSurfaceItem,
  InterfaceChromeIntent,
  SettingsIntent
}
import com.serenity.config.{
  AppConfig,
  ConfigManager,
  ConfigManagerTestSupport,
  ConfigVersion,
  StatusLinePlacement,
  UiLayout
}
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.{ConfirmAction, ConfirmPrompt, Modal, SurfaceContent}
import com.serenity.testkit.SharedDictionary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerUiLayoutSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def editorOn(configPath: Path): StateManager =
    val initial = ConfigManager.loadConfigIO(Some(configPath.toString)).unsafeRunSync()
    StateManager
      .apply(
        testLogger("StateManagerUiLayoutSpec"),
        sessionRootOverride = Some(TestTemp.directory("ui-layout-session")),
        initialConfig = initial,
        configPersistencePath = Some(configPath),
        dictionaryCache = SharedDictionary.cacheFor(initial)
      )
      .unsafeRunSync()

  private def configFile(text: String): Path =
    val path = TestTemp.directory("ui-layout-config").resolve("config.conf")
    Files.writeString(path, s"config.version = ${ConfigVersion.Current.value}\n$text")
    path

  private def toggleLayout(stateManager: StateManager): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          "toggle-layout",
          "Switch between the minimal and conventional layouts.",
          CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.ToggleUiLayout)),
          CommandCategory.View
        )
      )
      .unsafeRunSync()
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()

  private def assigns(text: String, key: String): Boolean =
    text.linesIterator.map(_.trim).exists(_.startsWith(s"$key ="))

  private def live(stateManager: StateManager): AppConfig =
    stateManager.getCurrentState.unsafeRunSync().persisted.config

  private def prompts(stateManager: StateManager): List[ConfirmPrompt] =
    val state = stateManager.getCurrentState.unsafeRunSync()
    state.runtime.modalStack.map(_.modal).collect { case Modal.Confirm(prompt) => prompt } ++
      state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.Confirm(p)) => p }

  private def run(stateManager: StateManager, intent: InterfaceChromeIntent): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          "layout-test",
          "Change the layout",
          CommandIntent.Settings(SettingsIntent.InterfaceChrome(intent)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()

  /** Does what choosing `label` in the one open prompt does, as the modal would once it closed. */
  private def choose(stateManager: StateManager, label: String): Unit =
    val chosen = prompts(stateManager).flatMap(_.choices.items).find(_.label == label)
    chosen.map(_.action) match
      case Some(ConfirmAction.Run(command)) =>
        stateManager.executeCommand(command).unsafeRunSync()
        stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()
      case other => fail(s"expected a runnable choice named $label, got $other")

  private val pinnedFile = "editor.line_numbers = false\nstatus.placement = floating\neditor.tab_size = 3\n"

  "toggle-layout" should "be a registered command" in {
    com.serenity.command.CommandRegistry.withToggleUI.getAllCommands.map(_.name) should contain("toggle-layout")
  }

  it should "switch a minimal layout to the conventional one and write only the layout key" in {
    val path         = configFile("editor.tab_size = 3\n")
    val stateManager = editorOn(path)

    toggleLayout(stateManager)

    live(stateManager) shouldBe ConfigManagerTestSupport.loadConfig(Some(path.toString))
    live(stateManager).uiLayout shouldBe UiLayout.Conventional
    live(stateManager).surfaceConfig.showLineNumbers shouldBe true
    live(stateManager).statusLine.placement shouldBe StatusLinePlacement.Pinned
    val text = Files.readString(path)
    text should include("ui.layout = conventional")
    assigns(text, "editor.line_numbers") shouldBe false
    assigns(text, "status.placement") shouldBe false
  }

  it should "switch back again" in {
    val path         = configFile("ui.layout = conventional\n")
    val stateManager = editorOn(path)

    toggleLayout(stateManager)

    live(stateManager).uiLayout shouldBe UiLayout.Minimal
    live(stateManager).surfaceConfig.showLineNumbers shouldBe false
    Files.readString(path) should include("ui.layout = minimal")
  }

  it should "ask before changing chrome the file sets, and leave the layout alone until asked" in {
    val path         = configFile(pinnedFile)
    val stateManager = editorOn(path)

    toggleLayout(stateManager)

    live(stateManager).uiLayout shouldBe UiLayout.Minimal
    Files.readString(path) should not include "ui.layout = conventional"
    val prompt = prompts(stateManager).headOption.getOrElse(fail("expected a layout prompt"))
    prompt.message should contain("Editor Line Numbers: false -> true")
    prompt.message should contain("Status Placement: floating -> pinned")
    prompt.choices.items.map(_.label) shouldBe Vector(
      "Use Conventional for all",
      "Keep my settings",
      "Choose one by one",
      "Cancel"
    )
  }

  it should "leave a setting the file writes where the file put it when told to keep it" in {
    val path         = configFile(pinnedFile)
    val stateManager = editorOn(path)

    toggleLayout(stateManager)
    choose(stateManager, "Keep my settings")

    live(stateManager).uiLayout shouldBe UiLayout.Conventional
    live(stateManager).surfaceConfig.showLineNumbers shouldBe false
    live(stateManager).statusLine.placement shouldBe StatusLinePlacement.Floating
    live(stateManager).surfaceConfig.showPaneHeaders shouldBe true
    val text = Files.readString(path)
    text should include("editor.line_numbers = false")
    text should include("status.placement = floating")
    assigns(text, "editor.pane_headers") shouldBe false
  }

  it should "hand every pinned key back to the layout when told to use the layout for all" in {
    val path         = configFile(pinnedFile)
    val stateManager = editorOn(path)

    toggleLayout(stateManager)
    choose(stateManager, "Use Conventional for all")

    live(stateManager).uiLayout shouldBe UiLayout.Conventional
    live(stateManager).surfaceConfig.showLineNumbers shouldBe true
    live(stateManager).statusLine.placement shouldBe StatusLinePlacement.Pinned
    val text = Files.readString(path)
    text should include("ui.layout = conventional")
    text should include("editor.tab_size = 3")
    assigns(text, "editor.line_numbers") shouldBe false
    assigns(text, "status.placement") shouldBe false
    live(stateManager) shouldBe ConfigManagerTestSupport.loadConfig(Some(path.toString))
  }

  it should "let a released key follow the layout on the next switch, without asking again" in {
    val path         = configFile(pinnedFile)
    val stateManager = editorOn(path)
    toggleLayout(stateManager)
    choose(stateManager, "Use Conventional for all")

    toggleLayout(stateManager)

    live(stateManager).uiLayout shouldBe UiLayout.Minimal
    live(stateManager).surfaceConfig.showLineNumbers shouldBe false
    live(stateManager).statusLine.placement shouldBe StatusLinePlacement.Off
    ConfigManagerTestSupport.loadConfig(Some(path.toString)) shouldBe live(stateManager)
  }

  it should "release only the keys chosen one by one" in {
    val path         = configFile(pinnedFile)
    val stateManager = editorOn(path)

    run(stateManager, InterfaceChromeIntent.ConfirmUiLayoutOverride(UiLayout.Conventional, List("status.placement")))

    live(stateManager).uiLayout shouldBe UiLayout.Conventional
    live(stateManager).surfaceConfig.showLineNumbers shouldBe false
    live(stateManager).statusLine.placement shouldBe StatusLinePlacement.Pinned
    val text = Files.readString(path)
    text should include("editor.line_numbers = false")
    assigns(text, "status.placement") shouldBe false
  }

  it should "open the preset diff review, one toggle per pinned key, when asked to choose one by one" in {
    val path         = configFile(pinnedFile)
    val stateManager = editorOn(path)

    toggleLayout(stateManager)
    choose(stateManager, "Choose one by one")

    val runner = stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.map(_.content) match
      case Some(SurfaceContent.CommandPalette(runner)) => runner
      case other                                       => fail(s"expected the command runner, got $other")
    runner.surface shouldBe a[CommandRunnerSurface.PresetDiffReview]
    runner.visibleItems.collect { case toggle: CommandSurfaceItem.ToggleItem => toggle.id } shouldBe
      List("editor.line_numbers", "status.placement")
    live(stateManager).uiLayout shouldBe UiLayout.Minimal
  }

  it should "ask in the same way when the settings row picks the layout" in {
    val path         = configFile(pinnedFile)
    val stateManager = editorOn(path)

    run(stateManager, InterfaceChromeIntent.SetUiLayout(UiLayout.Conventional))

    live(stateManager).uiLayout shouldBe UiLayout.Minimal
    prompts(stateManager) should have size 1
  }

  it should "not ask about a pinned key that already says what the layout does" in {
    val path         = configFile("editor.line_numbers = true\n")
    val stateManager = editorOn(path)

    toggleLayout(stateManager)

    prompts(stateManager) shouldBe Nil
    live(stateManager).uiLayout shouldBe UiLayout.Conventional
    Files.readString(path) should include("editor.line_numbers = true")
  }

  it should "still apply the layout's value to a key it could not remove from the file" in {
    val path         = configFile("editor {\n  line_numbers = false\n}\n")
    val stateManager = editorOn(path)

    run(stateManager, InterfaceChromeIntent.ConfirmUiLayoutOverride(UiLayout.Conventional, List("editor.line_numbers")))

    live(stateManager).surfaceConfig.showLineNumbers shouldBe true
    ConfigManagerTestSupport.loadConfig(Some(path.toString)).surfaceConfig.showLineNumbers shouldBe true
  }

  it should "apply the layout the settings row picks" in {
    val path         = configFile("")
    val stateManager = editorOn(path)

    stateManager
      .executeCommand(
        Command.typed(
          "set-layout",
          "Set the layout",
          CommandIntent.Settings(
            SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetUiLayout(UiLayout.Conventional))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()

    live(stateManager).uiLayout shouldBe UiLayout.Conventional
    live(stateManager).surfaceConfig.scrollbarEnabled shouldBe true
  }
