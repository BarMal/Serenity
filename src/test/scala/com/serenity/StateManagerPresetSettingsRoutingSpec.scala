package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Issue #1682: the settings pages under Edit Preset X change preset X, never the live settings. */
class StateManagerPresetSettingsRoutingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val DraftingFontSize = 19.0f
  private val EditedFontSize   = 23.0f

  private def managerWithStore(store: UiPresetStore): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerPresetSettingsRoutingSpec"))
    StateManager
      .apply(logger, uiPresetStore = store, dictionaryCache = SharedDictionary.default)
      .unsafeRunSync()

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def runnerOf(sm: StateManager): CommandRunner =
    sm.getCurrentState
      .map(
        _.commandRunnerSurface.flatMap {
          _.content match
            case SurfaceContent.CommandPalette(runner) => Some(runner)
            case _                                     => None
        }
      )
      .unsafeRunSync()
      .getOrElse(fail("command runner should be open"))

  private def globalConfig(sm: StateManager): AppConfig =
    sm.getCurrentState.unsafeRunSync().persisted.config

  private def pageItems(sm: StateManager, groupId: String): List[CommandSurfaceItem] =
    val groups = runnerOf(sm).settingsGroups
    val page = (groups ++ groups.flatMap(group =>
      descendants(group).collect { case child: CommandSurfaceItem.GroupItem => child }
    )).find(_.id == groupId).getOrElse(fail(s"missing group $groupId"))
    descendants(page)

  private def inputOn(sm: StateManager, groupId: String, itemSuffix: String): CommandSurfaceItem.InputItem =
    pageItems(sm, groupId)
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id.endsWith(itemSuffix) => item }
      .getOrElse(fail(s"missing input $itemSuffix on $groupId"))

  private def optionOn(sm: StateManager, groupId: String, itemSuffix: String): CommandSurfaceItem.OptionItem =
    pageItems(sm, groupId)
      .collectFirst { case item: CommandSurfaceItem.OptionItem if item.id.endsWith(itemSuffix) => item }
      .getOrElse(fail(s"missing option $itemSuffix on $groupId"))

  private def execute(sm: StateManager, id: String, label: String, intent: CommandIntent): Unit =
    sm.executeCommand(Command.typed(id, label, intent, CommandCategory.Settings)).unsafeRunSync()
    sm.runtimeLifecycle.awaitEffects.unsafeRunSync()

  private def submitInput(sm: StateManager, item: CommandSurfaceItem.InputItem, text: String): Unit =
    val intent = item.parse(text).getOrElse(fail(s"'$text' should parse for ${item.id}"))
    execute(sm, item.id, item.label, intent)

  private def chooseOption(sm: StateManager, item: CommandSurfaceItem.OptionItem, label: String): Unit =
    val option = item.options.find(_.label == label).getOrElse(fail(s"missing option $label on ${item.id}"))
    execute(sm, item.id, item.label, option.intent)

  private def drafting: UiPreset =
    val base = AppConfig.default
    UiPreset(
      name = "Drafting",
      config = base
        .withFontConfig(base.editorConfig.fontConfig.copy(fontSize = DraftingFontSize))
        .withSpellCheck(base.languageToolsConfig.spellCheck.copy(enabled = true))
        .withDefaultDocumentMode(DefaultDocumentMode.PlainText)
    )

  private def openedOnDrafting(prefix: String): (StateManager, UiPresetStore) =
    val store = UiPresetStore(TestTemp.directory(prefix).resolve("ui-presets.json"))
    val sm    = managerWithStore(store)
    store.upsert(drafting).unsafeRunSync()
    store.upsert(drafting.copy(name = "Second")).unsafeRunSync()
    (sm.applyEvent(ToggleCommandRunner) >> sm.runtimeLifecycle.awaitEffects).unsafeRunSync()
    execute(
      sm,
      "ui-preset-clear-theme",
      "Clear Preset Theme",
      CommandIntent.UiPresets(UiPresetsIntent.ClearUiPresetTheme("Drafting"))
    )
    (sm, store)

  "Editing a preset's settings" should "change a font size on that preset, persist it, and leave the live config alone" in {
    val (sm, store) = openedOnDrafting("preset-routing-font-size")
    val liveBefore  = globalConfig(sm)

    submitInput(sm, inputOn(sm, "settings-preset-code-font", "code-font-size"), EditedFontSize.toString)

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting should still exist"))
    saved.config.editorConfig.fontConfig.codeFontSize shouldBe EditedFontSize
    globalConfig(sm) shouldBe liveBefore
  }

  it should "change a document default on that preset and leave the live config alone" in {
    val (sm, store) = openedOnDrafting("preset-routing-document-default")
    val liveBefore  = globalConfig(sm)

    chooseOption(sm, optionOn(sm, "settings-preset-document-defaults", "default-document-mode"), "Rich Text")

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting should still exist"))
    saved.config.defaultDocumentMode shouldBe DefaultDocumentMode.RichText
    globalConfig(sm) shouldBe liveBefore
  }

  it should "change spell check on that preset and leave the live config alone" in {
    val (sm, store) = openedOnDrafting("preset-routing-spellcheck")
    val liveBefore  = globalConfig(sm)

    chooseOption(sm, optionOn(sm, "settings-preset-spellcheck", "spellcheck-enabled"), "Off")

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting should still exist"))
    saved.config.languageToolsConfig.spellCheck.enabled shouldBe false
    globalConfig(sm) shouldBe liveBefore
  }

  it should "change a font family on that preset and leave the live config alone" in {
    val (sm, store) = openedOnDrafting("preset-routing-font-family")
    val liveBefore  = globalConfig(sm)

    val familyPicker = pageItems(sm, "settings-preset-code-font")
      .collect { case item: CommandSurfaceItem.CommandItem => item }
      .lastOption
      .getOrElse(fail("the code font picker should list at least one family"))
    execute(sm, familyPicker.command.name, familyPicker.command.label, familyPicker.command.intent)

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting should still exist"))
    saved.config.editorConfig.fontConfig.codeFontFamily shouldBe familyPicker.command.label
    globalConfig(sm) shouldBe liveBefore
  }

  it should "leave every other preset untouched" in {
    val (sm, store)  = openedOnDrafting("preset-routing-siblings")
    val secondBefore = store.find("Second").unsafeRunSync()

    submitInput(sm, inputOn(sm, "settings-preset-code-font", "code-font-size"), EditedFontSize.toString)

    store.find("Second").unsafeRunSync() shouldBe secondBefore
  }

  it should "show the edited value on the page once the preset has been saved" in {
    val (sm, _) = openedOnDrafting("preset-routing-page-refresh")

    submitInput(sm, inputOn(sm, "settings-preset-code-font", "code-font-size"), EditedFontSize.toString)

    inputOn(sm, "settings-preset-code-font", "code-font-size").currentValue shouldBe EditedFontSize.toString
  }

  it should "refuse a built-in preset and change nothing" in {
    val store = UiPresetStore(TestTemp.directory("preset-routing-built-in").resolve("ui-presets.json"))
    val sm    = managerWithStore(store)
    (sm.applyEvent(ToggleCommandRunner) >> sm.runtimeLifecycle.awaitEffects).unsafeRunSync()
    execute(
      sm,
      "ui-preset-overwrite",
      "Overwrite",
      CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Writing"))
    )
    runnerOf(sm).editingPresetName shouldBe Some("Writing")
    val liveBefore = globalConfig(sm)

    submitInput(sm, inputOn(sm, "settings-preset-code-font", "code-font-size"), EditedFontSize.toString)

    store.list().unsafeRunSync() shouldBe Nil
    globalConfig(sm) shouldBe liveBefore
    runnerOf(sm).statusMessage shouldBe Some("Built-in preset cannot be edited. Duplicate Writing first.")
  }

  it should "report a preset that no longer exists and change nothing" in {
    val (sm, store) = openedOnDrafting("preset-routing-missing")
    val item        = inputOn(sm, "settings-preset-code-font", "code-font-size")
    val liveBefore  = globalConfig(sm)
    store.delete("Drafting").unsafeRunSync()

    submitInput(sm, item, EditedFontSize.toString)

    store.find("Drafting").unsafeRunSync() shouldBe None
    globalConfig(sm) shouldBe liveBefore
    runnerOf(sm).statusMessage shouldBe Some("Custom preset 'Drafting' was not found.")
  }

  "Editing the global settings" should "change the live config and create no preset" in {
    val store = UiPresetStore(TestTemp.directory("preset-routing-global").resolve("ui-presets.json"))
    val sm    = managerWithStore(store)
    (sm.applyEvent(ToggleCommandRunner) >> sm.runtimeLifecycle.awaitEffects).unsafeRunSync()

    execute(
      sm,
      "code-font-size",
      "Code Font Size",
      CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeFontSize(EditedFontSize)))
    )

    globalConfig(sm).editorConfig.fontConfig.codeFontSize shouldBe EditedFontSize
    store.list().unsafeRunSync() shouldBe Nil
  }

  it should "change the live config when the global page's own row is submitted" in {
    val store = UiPresetStore(TestTemp.directory("preset-routing-global-row").resolve("ui-presets.json"))
    val sm    = managerWithStore(store)
    (sm.applyEvent(ToggleCommandRunner) >> sm.runtimeLifecycle.awaitEffects).unsafeRunSync()

    submitInput(sm, inputOn(sm, "settings-code-font", "code-font-size"), EditedFontSize.toString)

    globalConfig(sm).editorConfig.fontConfig.codeFontSize shouldBe EditedFontSize
    store.list().unsafeRunSync() shouldBe Nil
  }

  it should "treat a global-targeted edit exactly like the edit it carries" in {
    val store = UiPresetStore(TestTemp.directory("preset-routing-global-target").resolve("ui-presets.json"))
    val sm    = managerWithStore(store)
    (sm.applyEvent(ToggleCommandRunner) >> sm.runtimeLifecycle.awaitEffects).unsafeRunSync()

    execute(
      sm,
      "code-font-size",
      "Code Font Size",
      CommandIntent.Scoped(
        SettingsTarget.Global,
        CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeFontSize(EditedFontSize)))
      )
    )

    globalConfig(sm).editorConfig.fontConfig.codeFontSize shouldBe EditedFontSize
    store.list().unsafeRunSync() shouldBe Nil
  }
