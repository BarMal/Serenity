package com.serenity

import java.awt.Font
import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerUiPresetSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def managerWithStore(
    store: UiPresetStore,
    windowSize: IO[Option[PreferredWindowSize]] = IO.pure(None),
    onWindowSizeChanged: PreferredWindowSize => IO[Unit] = _ => IO.unit,
    sessionRoot: Option[Path] = None
  ): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerUiPresetSpec"))
    StateManager
      .apply(
        logger,
        uiPresetStore = store,
        windowSizeProvider = windowSize,
        onPreferredWindowSizeChanged = onWindowSizeChanged,
        sessionRootOverride = sessionRoot
      )
      .unsafeRunSync()

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def commandRunnerState(sm: StateManager): com.serenity.command.CommandRunner =
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

  "StateManager UI presets" should "save the current UI preset to the preset store" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-save").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val size  = PreferredWindowSize(1500, 950)
    val sm    = managerWithStore(store, IO.pure(Some(size)))

    sm.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 12).unsafeRunSync()
    sm.updateState(state =>
      state.copy(
        persisted = state.persisted.copy(
          config = state.persisted.config.copy(backgroundStyle = BackgroundStyle.GlassLike),
          theme = Theme.light
        )
      )
    ).unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "save-workbench-preset",
        "Save workbench preset",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Workbench")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val saved = store.find("Workbench").unsafeRunSync()

    saved.map(_.themeName) shouldBe Some(Theme.light.name)
    saved.map(_.config.backgroundStyle) shouldBe Some(BackgroundStyle.GlassLike)
    saved.flatMap(_.config.preferredWindowSize) shouldBe Some(size)
    saved.map(_.pinnedPanels.map(panel => panel.position -> panel.size)) shouldBe Some(List(PanelPosition.Bottom -> 12))
  }

  it should "apply a named preset without resizing the live runtime window" in {
    val path               = Files.createTempDirectory("state-manager-ui-preset-apply").resolve("ui-presets.json")
    val store              = UiPresetStore(path)
    val observedWindowSize = Ref.of[IO, Option[PreferredWindowSize]](None).unsafeRunSync()
    val sm = managerWithStore(
      store,
      onWindowSizeChanged = size => observedWindowSize.set(Some(size))
    )
    val preset = com.serenity.ui.presets.UiPreset(
      name = "Review Custom",
      config = AppConfig.default
        .withBackgroundStyle(BackgroundStyle.Solid)
        .withPreferredWindowSize(PreferredWindowSize(1280, 720)),
      themeName = Theme.dark.name,
      pinnedPanels = List(
        com.serenity.ui.presets.UiPreset.PinnedPanel
          .fromPanelContent(
            PanelContent.Outline(Nil),
            PanelPosition.Right,
            36
          )
          .getOrElse(fail("outline should be capturable"))
      )
    )
    store.upsert(preset).unsafeRunSync()
    sm.handleViewportResize(ViewportSize(90, 28)).unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-review-preset",
        "Apply review preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Review Custom")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.persisted.config.backgroundStyle shouldBe BackgroundStyle.Solid
    state.persisted.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1280, 720))
    state.runtime.viewportSize shouldBe Some(ViewportSize(90, 28))
    state.persisted.theme.name shouldBe Theme.dark.name
    state.pinnedSurfaces.map(_.presentation) shouldBe List(SurfacePresentation.Pinned(PanelPosition.Right, 36))
    observedWindowSize.get.unsafeRunSync() shouldBe None
  }

  it should "apply a built-in writing preset when no custom preset exists" in {
    val path  = Files.createTempDirectory("state-manager-built-in-ui-preset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.updateState { state =>
      val primaryBufferId   = BufferId(0)
      val secondaryBufferId = BufferId(1)
      val pane0             = PaneId(0)
      val pane1             = PaneId(1)
      state.copy(
        persisted = state.persisted.copy(
          buffers = state.persisted.buffers + (secondaryBufferId -> Buffer.newEmpty(secondaryBufferId)),
          bufferOrder = List(primaryBufferId, secondaryBufferId),
          layout = Layout(
            editorPanes = Map(
              pane0 -> EditorPane.withBuffer(pane0, primaryBufferId),
              pane1 -> EditorPane.withBuffer(pane1, secondaryBufferId)
            ),
            activeEditorPaneId = Some(pane1),
            paneOrder = List(pane0, pane1)
          ),
          focus = Focus.EditorPane(pane1)
        ),
        runtime = state.runtime.copy(
          nextBufferId = BufferId(2),
          nextPaneId = PaneId(2)
        )
      )
    }.unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-writing-preset",
        "Apply writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.persisted.config.fontConfig.textFontFamily shouldBe Font.SERIF
    state.persisted.config.showLineNumbers shouldBe false
    state.persisted.config.showGutter shouldBe false
    state.persisted.layout.editorPanes should have size 1
    state.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(1))
    state.persisted.layout.editorPanes(PaneId(1)).bufferId shouldBe Some(BufferId(1))
    state.persisted.buffers(BufferId(1)).richText.richTextDocument should not be empty
    state.persisted.config.showPaneHeaders shouldBe false
    state.pinnedSurfaces shouldBe Nil
  }

  it should "keep Writing's new session in its single editor pane" in {
    val path  = Files.createTempDirectory("state-manager-writing-new-session").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.executeCommand(
      Command.typed(
        "apply-writing-preset",
        "Apply writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "startup-new-session",
        "Start a new session",
        CommandIntent.Session(SessionIntent.StartupNewSession)
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.persisted.layout.editorPanes should have size 1
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId) shouldBe
      state.persisted.bufferOrder.lastOption
    state.focusedBufferId.flatMap(state.persisted.buffers.get).flatMap(_.richText.richTextDocument) should not be empty
  }

  it should "preserve unrelated persisted configuration when applying a built-in workflow" in {
    val path  = Files.createTempDirectory("state-manager-built-in-workflow-config").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val lspConfig = LspUserConfig(
      Some(Map(LanguageId.Scala.id -> LspServerOverride(Some("scala-cli"), Some(List("lsp")), Some(false))))
    )
    val spellCheck = SpellCheckConfig(enabled = true, languages = List("en", "fr"))
    val windowConfig = WindowConfig(
      chromeMode = WindowChromeMode.NativeThemed,
      preferredSize = Some(PreferredWindowSize(1366, 768))
    )

    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(
          config = state.persisted.config
            .withHotkeyOverride(HotkeyAction.ToggleCommandRunner, "alt+p")
            .withKeymapBinding(KeymapGroup.Editor)(EditorKeyAction.MoveLeft, "alt+h")
            .withLanguageToolsConfig(
              state.persisted.config.languageToolsConfig.copy(lspUserConfig = lspConfig, spellCheck = spellCheck)
            )
            .withWindowConfig(windowConfig)
        )
      )
    }.unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-writing-preset",
        "Apply writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val config = sm.getCurrentState.unsafeRunSync().persisted.config

    config.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).map(_.render) shouldBe List("alt+p")
    config.inputConfig.focusedKeymapConfig.editor.bindingsFor(EditorKeyAction.MoveLeft).map(_.render) shouldBe List(
      "alt+h"
    )
    config.languageToolsConfig.lspUserConfig shouldBe lspConfig
    config.languageToolsConfig.spellCheck shouldBe spellCheck
    config.windowConfig shouldBe windowConfig
    config.showLineNumbers shouldBe false
    config.showPaneHeaders shouldBe false
  }

  it should "apply the built-in documentation preset to the active empty buffer" in {
    val path  = Files.createTempDirectory("state-manager-documentation-empty-ui-preset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.executeCommand(
      Command.typed(
        "apply-documentation-preset",
        "Apply documentation preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Documentation")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.persisted.buffers(BufferId(0)).document.language shouldBe Some(LanguageId.Markdown)
    state.pinnedSurfaces.collectFirst {
      case UiSurface(
            _,
            SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"),
            SurfacePresentation.Pinned(PanelPosition.Right, 40),
            _
          ) =>
        true
    } shouldBe Some(true)
  }

  it should "apply the built-in documentation preset with a live markdown preview for the active markdown buffer" in {
    val path  = Files.createTempDirectory("state-manager-documentation-ui-preset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.updateState { state =>
      val bufferId = BufferId(0)
      val buffer = state.persisted
        .buffers(bufferId)
        .copy(
          document = state.persisted
            .buffers(bufferId)
            .document
            .copy(
              content = Rope("# Notes\n\nDraft"),
              language = Some(LanguageId.Markdown)
            )
        )
      state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
    }.unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-documentation-preset",
        "Apply documentation preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Documentation")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.persisted.config.defaultDocumentMode shouldBe com.serenity.config.DefaultDocumentMode.Markdown
    state.pinnedSurfaces.collect { case UiSurface(_, SurfaceContent.Outline(_, _), _, _) => () } shouldBe Nil
    state.pinnedSurfaces.collectFirst {
      case UiSurface(
            _,
            SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"),
            SurfacePresentation.Pinned(PanelPosition.Right, 40),
            _
          ) =>
        true
    } shouldBe Some(true)
  }

  it should "leave the documentation outline optional for the active markdown buffer" in {
    val path  = Files.createTempDirectory("state-manager-documentation-outline-ui-preset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.updateState { state =>
      val bufferId = BufferId(0)
      val buffer = state.persisted
        .buffers(bufferId)
        .copy(
          document = state.persisted
            .buffers(bufferId)
            .document
            .copy(
              content = Rope("# Chapter One\n\nBody\n\n## Scene Two"),
              language = Some(LanguageId.Markdown)
            ),
          annotations = state.persisted
            .buffers(bufferId)
            .annotations
            .copy(
              bookmarks = List(CursorPosition(2, 4))
            )
        )
      state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
    }.unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-documentation-preset",
        "Apply documentation preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Documentation")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().pinnedSurfaces.collect {
      case UiSurface(_, SurfaceContent.Outline(_, _), _, _) => ()
    } shouldBe Nil
  }

  it should "hydrate the review preset outline from active bookmarks and headings" in {
    val path  = Files.createTempDirectory("state-manager-review-outline-ui-preset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.updateState { state =>
      val bufferId = BufferId(0)
      val buffer = state.persisted
        .buffers(bufferId)
        .copy(
          document = state.persisted
            .buffers(bufferId)
            .document
            .copy(
              content = Rope("# Finding\n\nNeeds review"),
              language = Some(LanguageId.Markdown)
            ),
          annotations = state.persisted
            .buffers(bufferId)
            .annotations
            .copy(
              bookmarks = List(CursorPosition(2, 0))
            )
        )
      state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
    }.unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-review-preset",
        "Apply review preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Review")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val outlineSymbols = state.pinnedSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.Outline(symbols, _), SurfacePresentation.Pinned(PanelPosition.Left, 30), _) =>
        symbols
    }

    outlineSymbols shouldBe Some(
      List(
        Symbol("Finding", SymbolKind.Heading, Location(0, 0)),
        Symbol("Bookmark 3:1", SymbolKind.Bookmark, Location(2, 0))
      )
    )
    state.pinnedSurfaces.map(_.content) should contain(SurfaceContent.Diagnostics(Nil))
  }

  it should "duplicate, rename, and delete UI presets from commands" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-management").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.executeCommand(
      Command.typed(
        "duplicate-writing-preset",
        "Duplicate writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset("Writing", "Personal Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Personal Writing").unsafeRunSync() should not be empty

    sm.executeCommand(
      Command.typed(
        "rename-writing-preset",
        "Rename writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset("Personal Writing", "Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Personal Writing").unsafeRunSync() shouldBe None
    store.find("Drafting").unsafeRunSync() should not be empty

    sm.executeCommand(
      Command.typed(
        "delete-writing-preset",
        "Delete writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    store.find("Drafting").unsafeRunSync() shouldBe None
    com.serenity.ui.presets.UiPreset.builtIn("Writing") should not be empty
  }

  it should "reject saving or duplicating over an existing preset name" in {
    val path     = Files.createTempDirectory("state-manager-ui-preset-name-collision").resolve("ui-presets.json")
    val store    = UiPresetStore(path)
    val existing = UiPreset("Drafting", AppConfig.default.withLineNumbers(false), Theme.dark.name, Nil)
    val sm       = managerWithStore(store)
    store.upsert(existing).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "save-as-new",
        "Save as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Drafting").unsafeRunSync() shouldBe Some(existing)
    commandRunnerState(sm).statusMessage.getOrElse(fail("save failure should be visible")) should include(
      "Could not save Drafting"
    )

    sm.executeCommand(
      Command.typed(
        "duplicate",
        "Duplicate",
        CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset("Writing", "Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Drafting").unsafeRunSync() shouldBe Some(existing)
    commandRunnerState(sm).statusMessage.getOrElse(fail("duplicate failure should be visible")) should include(
      "Could not duplicate Writing"
    )
  }

  it should "keep built-in presets immutable for rename commands" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-rename-built-in").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "rename-writing-preset",
        "Rename writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset("Writing", "Personal Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val runner = commandRunnerState(sm)

    store.find("Personal Writing").unsafeRunSync() shouldBe None
    runner.editingPresetName shouldBe Some("Writing")
    runner.statusMessage shouldBe Some("Built-in preset cannot be renamed. Duplicate Writing first.")
  }

  it should "overwrite a custom preset with the live workspace and refuse invalid overwrites" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-overwrite").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    store.upsert(UiPreset("Drafting", AppConfig.default.withLineNumbers(false), Theme.dark.name, Nil)).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "set-markdown-default",
        "Set document default",
        CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown)),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "overwrite",
        "Overwrite",
        CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))
    saved.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    commandRunnerState(sm).statusMessage shouldBe Some("Preset overwritten. Configure Drafting.")

    sm.executeCommand(
      Command.typed(
        "overwrite-built-in",
        "Overwrite",
        CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Writing").unsafeRunSync() shouldBe None
    commandRunnerState(sm).statusMessage shouldBe Some(
      "Built-in preset cannot be overwritten. Duplicate Writing first."
    )

    sm.executeCommand(
      Command.typed(
        "overwrite-missing",
        "Overwrite",
        CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Absent")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Absent").unsafeRunSync() shouldBe None
    commandRunnerState(sm).statusMessage shouldBe Some(
      "Custom preset 'Absent' was not found. Use Save As New Preset."
    )
  }

  it should "reject unavailable preset resources without changing the workspace" in {
    val path    = Files.createTempDirectory("state-manager-ui-preset-unavailable-resources").resolve("ui-presets.json")
    val store   = UiPresetStore(path)
    val sm      = managerWithStore(store)
    val initial = sm.getCurrentState.unsafeRunSync()
    store
      .upsert(UiPreset("Missing Theme", AppConfig.default.withLineNumbers(false), "not-installed", Nil))
      .unsafeRunSync()
    store
      .upsert(
        UiPreset(
          "Missing Font",
          AppConfig.default.copy(fontConfig = AppConfig.default.fontConfig.copy(textFontFamily = "not-installed")),
          Theme.dark.name,
          Nil
        )
      )
      .unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "apply",
        "Apply",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Missing Theme")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.config shouldBe initial.persisted.config
    commandRunnerState(sm).statusMessage.getOrElse(fail("missing preset error")) should include(
      "Theme 'not-installed' could not be loaded"
    )

    sm.executeCommand(
      Command.typed(
        "apply-font",
        "Apply",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Missing Font")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.config shouldBe initial.persisted.config
    commandRunnerState(sm).statusMessage.getOrElse(fail("missing preset error")) should include(
      "Preset requires unavailable text font 'not-installed'"
    )
  }

  it should "keep built-in presets immutable for delete commands" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-delete-built-in").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "delete-writing-preset",
        "Delete writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val runner = commandRunnerState(sm)

    store.find("Writing").unsafeRunSync() shouldBe None
    runner.editingPresetName shouldBe Some("Writing")
    runner.statusMessage shouldBe Some("Built-in preset cannot be deleted. Use Reset Preset to discard overrides.")
  }

  it should "list custom UI presets in the command runner when opened" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-list").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default
        .withMotionPreset(com.serenity.config.MotionPreset.Reduced)
        .withMaterialPreset(com.serenity.config.MaterialPreset.Solid),
      themeName = Theme.dark.name,
      pinnedPanels = List(
        UiPreset.PinnedPanel
          .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Right, 34)
          .getOrElse(fail("outline should be capturable"))
      )
    )
    store.upsert(preset).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val runner = sm.getCurrentState
      .map(
        _.commandRunnerSurface.flatMap {
          _.content match
            case SurfaceContent.CommandPalette(runner) => Some(runner)
            case _                                     => None
        }
      )
      .unsafeRunSync()
      .getOrElse(fail("command runner should be open"))
    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))
    val presetPicker = descendants(presetGroup)
      .collectFirst {
        case item: CommandSurfaceItem.OptionItem if item.id == "ui-preset-select" => item
      }
      .getOrElse(fail("missing preset picker"))

    presetPicker.options.map(_.label) should contain("Drafting")
    presetPicker.options.find(_.label == "Drafting").map(_.intent) shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Drafting"))
    )
    presetPicker.options.find(_.label == "Drafting").flatMap(_.hint) shouldBe Some(
      "plain text default; dark; reduced motion; fade text reveal; solid material; solid background; comfortable density; SansSerif 12pt prose; Right outline 34"
    )
  }

  it should "refresh custom UI presets in an open command runner after saving" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-refresh").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "save-drafting-preset",
        "Save drafting preset",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val runner = sm.getCurrentState
      .map(
        _.commandRunnerSurface.flatMap {
          _.content match
            case SurfaceContent.CommandPalette(runner) => Some(runner)
            case _                                     => None
        }
      )
      .unsafeRunSync()
      .getOrElse(fail("command runner should stay open"))
    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))
    val presetPicker = descendants(presetGroup)
      .collectFirst {
        case item: CommandSurfaceItem.OptionItem if item.id == "ui-preset-select" => item
      }
      .getOrElse(fail("missing preset picker"))

    presetPicker.options.map(_.label) should contain("Drafting")
    presetPicker.options.find(_.label == "Drafting").map(_.intent) shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Drafting"))
    )
  }

  it should "keep command runner preset context current after preset management actions" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-action-status").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      pinnedPanels = Nil
    )
    store.upsert(preset).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "duplicate-drafting-preset",
        "Duplicate drafting preset",
        CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset("Drafting", "Drafting Copy")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    store.find("Drafting Copy").unsafeRunSync() should not be empty
    commandRunnerState(sm).editingPresetName shouldBe Some("Drafting Copy")
    commandRunnerState(sm).statusMessage shouldBe Some("Preset duplicated. Configure Drafting Copy.")

    sm.executeCommand(
      Command.typed(
        "rename-drafting-copy-preset",
        "Rename drafting copy preset",
        CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset("Drafting Copy", "Final Draft")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    commandRunnerState(sm).editingPresetName shouldBe Some("Final Draft")
    commandRunnerState(sm).statusMessage shouldBe Some("Preset renamed. Configure Final Draft.")

    sm.executeCommand(
      Command.typed(
        "delete-final-draft-preset",
        "Delete final draft preset",
        CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset("Final Draft")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    commandRunnerState(sm).editingPresetName shouldBe None
    commandRunnerState(sm).statusMessage shouldBe Some("Preset deleted.")
  }

  it should "open preset options after saving a new UI preset from the command runner" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-create-options").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val runner = state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("command runner should stay open"))
    val submenu = state.runtime.uiSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _) =>
        groupId -> previewOnly
    }

    runner.activeSubmenu.map(_.groupId) shouldBe Some("settings-preset-edit")
    runner.activeSubmenu.flatMap(_.parentGroupId) shouldBe Some("settings-ui-presets")
    runner.editingPresetName shouldBe Some("Drafting")
    runner.statusMessage shouldBe Some("Preset saved. Configure Drafting.")
    state.persisted.focus shouldBe Focus.Surface(SurfaceId("command-runner-submenu"))
    submenu shouldBe Some("settings-preset-edit" -> false)
  }

  it should "leave a saved preset untouched while later settings change the live workspace" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-snapshot").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    List(
      CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown)),
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionPreset(MotionPreset.Subtle))),
      CommandIntent.Settings(
        SettingsIntent.General(GeneralSettingsIntent.SetBackgroundStyle(BackgroundStyle.GlassLike))
      ),
      CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextFontSize(18.0f))),
      CommandIntent.View(ViewIntent.PinOutlinePanel)
    ).zipWithIndex.foreach { (intent, index) =>
      sm.executeCommand(Command.typed(s"change-$index", "Change setting", intent, CommandCategory.Settings))
        .unsafeRunSync()
    }

    val state = sm.getCurrentState.unsafeRunSync()

    state.persisted.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    state.persisted.config.motionPreset shouldBe MotionPreset.Subtle
    state.persisted.config.backgroundStyle shouldBe BackgroundStyle.GlassLike
    state.persisted.config.fontConfig.textFontSize shouldBe 18.0f
    store.find("Drafting").unsafeRunSync() shouldBe Some(savedBefore)
  }

  it should "capture live config, theme, and panel changes when overwriting a preset" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-overwrite-capture").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "change-document-default",
        "Change document default",
        CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown)),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-outline",
        "Pin outline",
        CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right))),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.updateState(state => state.copy(persisted = state.persisted.copy(theme = Theme.light))).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "overwrite",
        "Overwrite",
        CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    saved.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    saved.themeName shouldBe Theme.light.name
    saved.pinnedPanels.map(_.position) shouldBe List(PanelPosition.Right)
  }

  it should "expose panel reorder commands in the preset active panels group" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-panel-order-menu").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-drafting-outline-right",
        "Pin drafting outline right",
        CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right))),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-drafting-diagnostics-right",
        "Pin drafting diagnostics right",
        CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Right))),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val runner      = commandRunnerState(sm)
    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))
    val activePanelsGroup = descendants(presetGroup)
      .collectFirst {
        case item: CommandSurfaceItem.GroupItem if item.id == "settings-preset-active-panels" => item
      }
      .getOrElse(fail("missing active panels group"))
    val commands = descendants(activePanelsGroup).collect {
      case item: CommandSurfaceItem.CommandItem =>
        item.command.label -> item.command.intent
    }

    commands should contain(
      "Move Outline Earlier" -> CommandIntent.View(ViewIntent.MovePanelEarlier(PanelKind.Outline))
    )
    commands should contain("Move Outline Later" -> CommandIntent.View(ViewIntent.MovePanelLater(PanelKind.Outline)))
    commands should contain(
      "Move Diagnostics Earlier" -> CommandIntent.View(ViewIntent.MovePanelEarlier(PanelKind.Diagnostics))
    )
    commands should contain(
      "Move Diagnostics Later" -> CommandIntent.View(ViewIntent.MovePanelLater(PanelKind.Diagnostics))
    )
  }

  it should "save the live workspace under a second name without touching the first preset" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-second-name").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-rich-text-default",
        "Set drafting document default",
        CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText)),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "save-drafting-copy",
        "Save drafting copy",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting Edited")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val original = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))
    val copy     = store.find("Drafting Edited").unsafeRunSync().getOrElse(fail("Drafting Edited preset should exist"))
    val runner   = commandRunnerState(sm)

    original shouldBe savedBefore
    copy.config.defaultDocumentMode shouldBe DefaultDocumentMode.RichText
    runner.editingPresetName shouldBe Some("Drafting Edited")
    runner.statusMessage shouldBe Some("Preset saved. Configure Drafting Edited.")
  }

  it should "reset a custom built-in preset override to the built-in defaults" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-reset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    sm.executeCommand(
      Command.typed(
        "reset-writing-preset",
        "Reset writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.ResetUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "apply-writing-preset",
        "Apply writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    store.find("Writing").unsafeRunSync() shouldBe None
    state.persisted.config.fontConfig.textFontFamily shouldBe Font.SERIF
    state.pinnedSurfaces shouldBe Nil
  }
