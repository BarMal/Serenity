package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.AppConfigOps.*
import com.serenity.config.{
  AppConfig,
  AppMode,
  ConfigManager,
  HeadingLevelConfig,
  HeadingsConfig,
  PanelEscapeTarget,
  PerMode,
  WindowChromeMode
}
import com.serenity.richtext.ParagraphAlignment
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exercises [[StateManagerConfigEffects]] on its own: a real config path in a temp directory and an
  * [[EffectEditorPort]] double, so each settings intent can be checked for the config it writes *and* the collaborator
  * it calls.
  */
class StateManagerConfigEffectsSpec extends AnyFlatSpec with Matchers with ConfigEffectsHarness:

  "StateManagerConfigEffects" should "apply a config update to state and persist it" in {
    val fixture = harness()

    fixture.config.updateConfig(_.withWheelScrollLines(7)).unsafeRunSync().inputConfig.wheelScrollLines shouldBe 7

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 7
    fixture.configPath.map(path => Files.exists(path)) shouldBe Some(true)
  }

  it should "keep the terminal's hotkey adjustment on the config it commits, so the writer measures from the right defaults" in {
    val terminalConfig =
      AppConfig.default.withHotkeyConfig(com.serenity.config.HotkeyConfig.forOs("Mac OS X").forTerminalUse)
    val fixture = harness(initialState = AppState.initial(terminalConfig))

    fixture.config.updateConfig(_.withWheelScrollLines(7)).unsafeRunSync()

    fixture.currentConfig.inputConfig.hotkeyConfig.terminalAdjusted shouldBe true
  }

  it should "keep the terminal's hotkey adjustment when the settings are reset" in {
    val terminalConfig =
      AppConfig.default.withHotkeyConfig(com.serenity.config.HotkeyConfig.forOs("Mac OS X").forTerminalUse)
    val fixture = harness(initialState = AppState.initial(terminalConfig))

    fixture.config
      .interpret(
        SettingsIntent.General(GeneralSettingsIntent.ResetSettings),
        fixture.stateRef.get.unsafeRunSync()
      )
      .unsafeRunSync()

    val hotkeys = fixture.currentConfig.inputConfig.hotkeyConfig
    hotkeys.terminalAdjusted shouldBe true
    hotkeys.bindingsFor(com.serenity.config.HotkeyAction.Save).map(_.render) shouldBe List("ctrl+s")
  }

  it should "apply a config update to state when no config file is configured" in {
    val fixture = harness(persistConfig = false)

    fixture.config.updateConfig(_.withWheelScrollLines(3)).unsafeRunSync()

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 3
  }

  it should "resolve the device text scale and report the resulting font config" in {
    val fixture = harness(deviceTextScale = 2.0)

    fixture.config
      .updateFontConfig(_.copy(textScaleMode = FontLoader.TextScaleMode.Auto))
      .unsafeRunSync()

    val reported = fixture.fontConfigs.get.unsafeRunSync()
    reported.size shouldBe 1
    reported.head shouldBe fixture.currentConfig.editorConfig.fontConfig
    reported.head.textScaleMultiplier shouldBe
      FontLoader.FontConfig.clampTextScale(2.0)
  }

  it should "clamp font sizes at both ends of the supported range" in {
    val fixture = harness()

    fixture.config.interpret(SettingsIntent.Font(FontIntent.SetFontSize(200.0f)), AppState.initial).unsafeRunSync()
    fixture.currentConfig.editorConfig.fontConfig.fontSize shouldBe 48.0f

    fixture.config.interpret(SettingsIntent.Font(FontIntent.SetFontSize(1.0f)), AppState.initial).unsafeRunSync()
    fixture.currentConfig.editorConfig.fontConfig.fontSize shouldBe 8.0f
  }

  it should "reschedule document analysis after a spell-check setting changes" in {
    val fixture = harness()

    fixture.config
      .interpret(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckEnabled(true)), AppState.initial)
      .unsafeRunSync()

    fixture.currentConfig.languageToolsConfig.spellCheck.enabled shouldBe true
    fixture.analysisRuns.get.unsafeRunSync() shouldBe 1
  }

  it should "add the flagged word at the cursor to the custom spell-check dictionary" in {
    val text       = "hello wurld today"
    val config     = com.serenity.config.SpellCheckConfig(enabled = true, languages = List("en"))
    val bufferId   = BufferId(0)
    val baseBuffer = AppState.initial.persisted.buffers(bufferId)
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(content = com.serenity.rope.Rope(text)),
      editing = EditingState(List(CursorPosition(0, 8)))
    )
    val uri         = com.serenity.spellcheck.SpellChecker.diagnosticsUri(buffer)
    val diagnostics = com.serenity.spellcheck.SpellChecker.check(text, config)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> buffer)
      ),
      runtime = AppState.initial.runtime.copy(
        languageService = AppState.initial.runtime.languageService.copy(diagnosticsState =
          AppState.initial.runtime.languageService.diagnosticsState.copy(diagnostics = Map(uri -> diagnostics))
        )
      )
    )
    val fixture = harness(state)

    fixture.config
      .interpret(SettingsIntent.SpellCheck(SpellCheckIntent.AddWordAtCursorToDictionary), state)
      .unsafeRunSync()

    fixture.currentConfig.languageToolsConfig.spellCheck.additionalWords shouldBe List("wurld")
  }

  it should "not add a word to the dictionary when the cursor is not on a flagged word" in {
    val fixture = harness()

    fixture.config
      .interpret(SettingsIntent.SpellCheck(SpellCheckIntent.AddWordAtCursorToDictionary), AppState.initial)
      .unsafeRunSync()

    fixture.currentConfig.languageToolsConfig.spellCheck.additionalWords shouldBe Nil
  }

  // A diagnostic computed before "wurld" was added to the dictionary, still cached against a buffer whose config
  // now already accepts the word (e.g. re-analysis hasn't caught up yet) -- appending must still dedupe rather
  // than double the entry.
  it should "not duplicate a word already in the custom spell-check dictionary" in {
    val text = "hello wurld today"
    val staleDiagnostic =
      com.serenity.spellcheck.SpellChecker
        .check(text, com.serenity.config.SpellCheckConfig(enabled = true, languages = List("en")))
    val config     = com.serenity.config.SpellCheckConfig(enabled = true, additionalWords = List("wurld"))
    val bufferId   = BufferId(0)
    val baseBuffer = AppState.initial.persisted.buffers(bufferId)
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(content = com.serenity.rope.Rope(text)),
      editing = EditingState(List(CursorPosition(0, 8)))
    )
    val uri = com.serenity.spellcheck.SpellChecker.diagnosticsUri(buffer)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> buffer)
      ),
      runtime = AppState.initial.runtime.copy(
        languageService = AppState.initial.runtime.languageService.copy(diagnosticsState =
          AppState.initial.runtime.languageService.diagnosticsState.copy(diagnostics = Map(uri -> staleDiagnostic))
        )
      )
    )
    val fixture = harness(state)

    fixture.config
      .interpret(SettingsIntent.SpellCheck(SpellCheckIntent.AddWordAtCursorToDictionary), state)
      .unsafeRunSync()

    fixture.currentConfig.languageToolsConfig.spellCheck.additionalWords shouldBe List("wurld")
  }

  it should "route the contextual toolbar toggle through the editor event queue rather than the config" in {
    val fixture = harness()

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleContextualToolbar), AppState.initial)
      .unsafeRunSync()

    fixture.events.get.unsafeRunSync() shouldBe List(com.serenity.keystroke.events.ToggleContextualToolbar)
  }

  it should "propagate a contextual toolbar display mode change into the live toolbar surface" in {
    val toolbarId = SurfaceId("toolbar")
    val toolbar   = ContextualToolbarState()
    val state = AppState.initial.copy(
      runtime = AppState.initial.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            toolbarId,
            SurfaceContent.ContextualToolbar(toolbar),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val fixture = harness(state)
    val mode    = com.serenity.config.ToolbarDisplayMode.values.find(_ != toolbar.displayMode).get

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.SetContextualToolbarDisplayMode(mode)), state)
      .unsafeRunSync()

    fixture.stateRef.get.unsafeRunSync().surfaceById(toolbarId).map(_.content) shouldBe
      Some(SurfaceContent.ContextualToolbar(toolbar.copy(displayMode = mode)))
  }

  it should "toggle column mode" in {
    val fixture = harness()

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleColumnMode), AppState.initial)
      .unsafeRunSync()
    fixture.currentConfig.surfaceConfig.columnModeEnabled shouldBe true

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleColumnMode), AppState.initial)
      .unsafeRunSync()
    fixture.currentConfig.surfaceConfig.columnModeEnabled shouldBe false
  }

  it should "set column mode, target width, and gap directly" in {
    val fixture = harness()

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnMode(true)), AppState.initial)
      .unsafeRunSync()
    fixture.currentConfig.surfaceConfig.columnModeEnabled shouldBe true

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnTargetWidth(60)), AppState.initial)
      .unsafeRunSync()
    fixture.currentConfig.surfaceConfig.columnTargetWidthCells shouldBe 60

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnGap(4)), AppState.initial)
      .unsafeRunSync()
    fixture.currentConfig.surfaceConfig.columnGap shouldBe 4
  }

  it should "set an explicit column count and clear it back to Auto" in {
    val fixture = harness()

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(Some(3))), AppState.initial)
      .unsafeRunSync()
    fixture.currentConfig.surfaceConfig.columnCount shouldBe Some(3)

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(None)), AppState.initial)
      .unsafeRunSync()
    fixture.currentConfig.surfaceConfig.columnCount shouldBe None
  }

  it should "set drop caps enabled" in {
    val fixture = harness()

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.SetDropCapsEnabled(false)), AppState.initial)
      .unsafeRunSync()
    fixture.currentConfig.documentConfig.dropCapsEnabled shouldBe false

    fixture.config
      .interpret(SettingsIntent.TextDisplay(TextDisplayIntent.SetDropCapsEnabled(true)), AppState.initial)
      .unsafeRunSync()
    fixture.currentConfig.documentConfig.dropCapsEnabled shouldBe true
  }

  private def interpretHeadings(fixture: Harness, intents: HeadingsIntent*): Unit =
    intents.foreach(intent =>
      fixture.config.interpret(SettingsIntent.Headings(intent), AppState.initial).unsafeRunSync()
    )

  it should "set each heading style for one level, reaching the effective heading settings" in {
    val fixture = harness()

    interpretHeadings(
      fixture,
      HeadingsIntent.SetLevel(1, HeadingLevelChange.Size(Some(2.0f))),
      HeadingsIntent.SetLevel(1, HeadingLevelChange.Bold(Some(false))),
      HeadingsIntent.SetLevel(1, HeadingLevelChange.Italic(Some(true))),
      HeadingsIntent.SetLevel(1, HeadingLevelChange.Family(Some("Serif"))),
      HeadingsIntent.SetLevel(1, HeadingLevelChange.Align(Some(ParagraphAlignment.Center)))
    )

    val config = fixture.currentConfig
    config.headingsConfig.levels.head shouldBe HeadingLevelConfig(
      size = Some(2.0f),
      bold = Some(false),
      italic = Some(true),
      family = Some("Serif"),
      align = Some(ParagraphAlignment.Center)
    )
    config.headingsConfig.levels.tail.foreach(_ shouldBe HeadingLevelConfig())
    config.headingSettings.textStyle(1) shouldBe
      TextStyle(isBold = false, isItalic = true, fontFamily = Some("Serif"), fontSize = Some(24.0f))
    config.headingSettings.alignment(1) shouldBe Some(ParagraphAlignment.Center)
    config.headingSettings.textStyle(2) shouldBe AppConfig.default.headingSettings.textStyle(2)
  }

  it should "put a heading level back to auto" in {
    val fixture = harness()

    interpretHeadings(
      fixture,
      HeadingsIntent.SetLevel(3, HeadingLevelChange.Size(Some(2.0f))),
      HeadingsIntent.SetLevel(3, HeadingLevelChange.Size(None))
    )

    fixture.currentConfig.headingsConfig shouldBe HeadingsConfig()
  }

  it should "set heading damping and its threshold" in {
    val fixture = harness()

    interpretHeadings(fixture, HeadingsIntent.SetDamping(false), HeadingsIntent.SetDampingThreshold(40.0f))

    fixture.currentConfig.headingsConfig shouldBe HeadingsConfig(damping = false, dampingThresholdPt = 40.0f)
  }

  it should "change only the heading keys that were chosen in the config file" in {
    val fixture                                  = harness()
    def headingLines(text: String): List[String] = text.linesIterator.map(_.trim).filter(_.contains("headings.")).toList

    interpretHeadings(fixture, HeadingsIntent.SetLevel(3, HeadingLevelChange.Bold(Some(false))))

    val written = fixture.configPath.map(Files.readString).getOrElse(fail("expected a config file"))
    val before  = headingLines(textOf(AppConfig.default))
    headingLines(written).diff(before) shouldBe List("headings.h3.bold = false")
    before.diff(headingLines(written)) shouldBe List("headings.h3.bold = auto")
  }

  it should "set where Escape from a focused panel returns focus for one app mode only" in {
    val fixture = harness()

    fixture.config
      .interpret(
        SettingsIntent.InterfaceChrome(
          InterfaceChromeIntent.SetPanelEscapeTarget(AppMode.Prose, PanelEscapeTarget.Previous)
        ),
        AppState.initial
      )
      .unsafeRunSync()

    fixture.currentConfig.inputConfig.panelEscapeReturnsTo shouldBe
      PerMode(code = PanelEscapeTarget.Editor, prose = PanelEscapeTarget.Previous)
  }

  it should "write the current config to disk on an explicit save" in {
    val fixture = harness()

    fixture.config.persistConfigFile(AppConfig.default.withWheelScrollLines(9)).unsafeRunSync()

    fixture.configPath.map(path => Files.exists(path)) shouldBe Some(true)
  }

  private def textOf(config: AppConfig): String = ConfigManager.configToString(config)

  private def noticeTexts(fixture: Harness): List[String] =
    fixture.notices.get.unsafeRunSync().map(_.message)

  "Reloading the config file" should "apply an outside edit to the live config" in {
    val fixture = harness(configOnDisk = Some(AppConfig.default))

    editExternally(fixture, textOf(AppConfig.default.withWheelScrollLines(7)))
    reload(fixture)

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 7
    noticeTexts(fixture) shouldBe Nil
  }

  it should "leave a change made here but not yet written alone when the watcher reports our own write" in {
    val fixture = harness()

    fixture.config.updateConfig(_.withWheelScrollLines(7)).unsafeRunSync()
    fixture.stateRef.update(StateManagerConfigEffects.configUpdated(_, _.withWheelScrollLines(9))).unsafeRunSync()
    reload(fixture)

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 9
    noticeTexts(fixture) shouldBe Nil
  }

  it should "keep a setting changed here over an outside edit made at the same time, and say so" in {
    val fixture = harness(configOnDisk = Some(AppConfig.default))

    fixture.stateRef.update(StateManagerConfigEffects.configUpdated(_, _.withWheelScrollLines(9))).unsafeRunSync()
    editExternally(fixture, textOf(AppConfig.default.withWheelScrollLines(7)))
    reload(fixture)

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 9
    noticeTexts(fixture).exists(_.contains("settings from this session were kept")) shouldBe true
  }

  it should "take the file over settings that could not be written, since nothing here is worth protecting" in {
    val fixture = harness()

    fixture.stateRef.update(StateManagerConfigEffects.configUpdated(_, _.withWheelScrollLines(9))).unsafeRunSync()
    editExternally(fixture, textOf(AppConfig.default.withWheelScrollLines(7)))
    reload(fixture)

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 7
  }

  it should "apply every valid setting of an edited file and name the invalid one in a notice" in {
    val fixture = harness(configOnDisk = Some(AppConfig.default))
    val edited = textOf(AppConfig.default.withLineNumbers(false))
      .replaceAll("(?m)^editor\\.wheel_scroll_lines = .*$", "editor.wheel_scroll_lines = lots")

    editExternally(fixture, edited)
    reload(fixture)

    fixture.currentConfig.surfaceConfig.showLineNumbers shouldBe false
    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe AppConfig.default.inputConfig.wheelScrollLines
    noticeTexts(fixture).exists(text =>
      text.contains("editor.wheel_scroll_lines") && text.contains("were kept")
    ) shouldBe true
  }

  it should "keep the settings in use and say so when the edited file cannot be parsed" in {
    val fixture = harness(configOnDisk = Some(AppConfig.default))

    editExternally(fixture, "editor { unclosed")
    reload(fixture)

    fixture.currentConfig shouldBe AppConfig.default
    noticeTexts(fixture).exists(text =>
      text.contains("could not be parsed") && text.contains("already in use")
    ) shouldBe true
  }

  it should "apply a setting that needs a restart and say that it does" in {
    val fixture = harness(configOnDisk = Some(AppConfig.default))
    val chrome  = WindowChromeMode.values.find(_ != AppConfig.default.windowChromeMode).get

    editExternally(fixture, textOf(AppConfig.default.withWindowChromeMode(chrome)))
    reload(fixture)

    fixture.currentConfig.windowChromeMode shouldBe chrome
    noticeTexts(fixture).exists(text => text.contains("window.chrome") && text.contains("restarting")) shouldBe true
  }

  it should "report the new font config to the runtime" in {
    val fixture = harness(configOnDisk = Some(AppConfig.default))
    val font    = AppConfig.default.editorConfig.fontConfig.copy(uiFontSize = 19.0f)

    editExternally(fixture, textOf(AppConfig.default.withFontConfig(font)))
    reload(fixture)

    fixture.fontConfigs.get.unsafeRunSync().map(_.uiFontSize) shouldBe List(19.0f)
  }

  it should "reschedule document analysis when the spell-check settings are edited" in {
    val fixture    = harness(configOnDisk = Some(AppConfig.default))
    val spellCheck = AppConfig.default.languageToolsConfig.spellCheck.copy(enabled = false)

    editExternally(fixture, textOf(AppConfig.default.withSpellCheck(spellCheck)))
    reload(fixture)

    fixture.analysisRuns.get.unsafeRunSync() shouldBe 1
  }

  it should "do nothing when the file was only reformatted" in {
    val fixture = harness(configOnDisk = Some(AppConfig.default))

    editExternally(fixture, textOf(AppConfig.default) + "# a note to self\n")
    reload(fixture)

    fixture.currentConfig shouldBe AppConfig.default
    fixture.committedStates.get.unsafeRunSync() shouldBe Nil
    noticeTexts(fixture) shouldBe Nil
  }

  it should "keep the settings in use when the file is gone" in {
    val fixture = harness(configOnDisk = Some(AppConfig.default))

    reload(fixture)

    fixture.currentConfig shouldBe AppConfig.default
    noticeTexts(fixture) shouldBe Nil
  }

  it should "have nothing to watch when this session keeps no config file" in {
    harness(persistConfig = false).config.watch shouldBe None
  }
