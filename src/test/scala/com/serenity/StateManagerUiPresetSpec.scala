package com.serenity

import java.awt.Font
import java.nio.file.{Files, Path}

import _root_.io.circe.syntax.*
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.lsp.config.LanguageId
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

  private def assertPresetMarkedUnsaved(sm: StateManager, sourceName: String = "Drafting"): Unit =
    val runner = commandRunnerState(sm)

    runner.editingPresetName shouldBe Some(sourceName)
    runner.statusMessage shouldBe Some(
      "Preset draft has unsaved changes. Save commits them; Discard restores the workspace."
    )

  "StateManager UI presets" should "save the current UI preset to the preset store" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-save").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val size  = PreferredWindowSize(1500, 950)
    val sm    = managerWithStore(store, IO.pure(Some(size)))

    sm.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 12).unsafeRunSync()
    sm.updateState(state =>
      state.copy(
        config = state.config.copy(backgroundStyle = BackgroundStyle.GlassLike),
        theme = Theme.light
      )
    ).unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "save-workbench-preset",
        "Save workbench preset",
        CommandIntent.SaveUiPreset("Workbench"),
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
        CommandIntent.ApplyUiPreset("Review Custom"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.config.backgroundStyle shouldBe BackgroundStyle.Solid
    state.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1280, 720))
    state.viewportSize shouldBe Some(ViewportSize(90, 28))
    state.theme.name shouldBe Theme.dark.name
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
        buffers = state.buffers + (secondaryBufferId -> Buffer.newEmpty(secondaryBufferId)),
        bufferOrder = List(primaryBufferId, secondaryBufferId),
        layout = Layout(
          editorPanes = Map(
            pane0 -> EditorPane.withBuffer(pane0, primaryBufferId),
            pane1 -> EditorPane.withBuffer(pane1, secondaryBufferId)
          ),
          activeEditorPaneId = Some(pane1),
          paneOrder = List(pane0, pane1)
        ),
        focus = Focus.EditorPane(pane1),
        nextBufferId = BufferId(2),
        nextPaneId = PaneId(2)
      )
    }.unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-writing-preset",
        "Apply writing preset",
        CommandIntent.ApplyUiPreset("Writing"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.config.fontConfig.textFontFamily shouldBe Font.SERIF
    state.config.showLineNumbers shouldBe false
    state.config.showGutter shouldBe false
    state.layout.editorPanes should have size 1
    state.layout.activeEditorPaneId shouldBe Some(PaneId(1))
    state.layout.editorPanes(PaneId(1)).bufferId shouldBe Some(BufferId(1))
    state.buffers(BufferId(1)).richTextDocument should not be empty
    state.pinnedSurfaces.map(_.presentation) shouldBe List(SurfacePresentation.Pinned(PanelPosition.Left, 28))
    state.pinnedSurfaces.headOption.map(_.content) shouldBe Some(SurfaceContent.Outline(Nil))
  }

  it should "apply the built-in documentation preset to the active empty buffer" in {
    val path  = Files.createTempDirectory("state-manager-documentation-empty-ui-preset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.executeCommand(
      Command.typed(
        "apply-documentation-preset",
        "Apply documentation preset",
        CommandIntent.ApplyUiPreset("Documentation"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.buffers(BufferId(0)).language shouldBe Some(LanguageId.Markdown)
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
      val buffer = state
        .buffers(bufferId)
        .copy(
          content = Rope("# Notes\n\nDraft"),
          language = Some(LanguageId.Markdown)
        )
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-documentation-preset",
        "Apply documentation preset",
        CommandIntent.ApplyUiPreset("Documentation"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.config.defaultDocumentMode shouldBe com.serenity.config.DefaultDocumentMode.Markdown
    state.pinnedSurfaces.map(_.content) should contain(
      SurfaceContent.Outline(List(Symbol("Notes", SymbolKind.Heading, Location(0, 0))), Some(Location(0, 0)))
    )
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

  it should "hydrate the documentation preset outline from the active markdown buffer" in {
    val path  = Files.createTempDirectory("state-manager-documentation-outline-ui-preset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.updateState { state =>
      val bufferId = BufferId(0)
      val buffer = state
        .buffers(bufferId)
        .copy(
          content = Rope("# Chapter One\n\nBody\n\n## Scene Two"),
          language = Some(LanguageId.Markdown),
          bookmarks = List(CursorPosition(2, 4))
        )
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-documentation-preset",
        "Apply documentation preset",
        CommandIntent.ApplyUiPreset("Documentation"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val outlineSymbols = sm.getCurrentState.unsafeRunSync().pinnedSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.Outline(symbols, _), SurfacePresentation.Pinned(PanelPosition.Left, 30), _) =>
        symbols
    }

    outlineSymbols shouldBe Some(
      List(
        Symbol("Chapter One", SymbolKind.Heading, Location(0, 0)),
        Symbol("Bookmark 3:5", SymbolKind.Bookmark, Location(2, 4)),
        Symbol("Scene Two", SymbolKind.Heading, Location(4, 0))
      )
    )
  }

  it should "hydrate the review preset outline from active bookmarks and headings" in {
    val path  = Files.createTempDirectory("state-manager-review-outline-ui-preset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.updateState { state =>
      val bufferId = BufferId(0)
      val buffer = state
        .buffers(bufferId)
        .copy(
          content = Rope("# Finding\n\nNeeds review"),
          language = Some(LanguageId.Markdown),
          bookmarks = List(CursorPosition(2, 0))
        )
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-review-preset",
        "Apply review preset",
        CommandIntent.ApplyUiPreset("Review"),
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
        CommandIntent.DuplicateUiPreset("Writing", "Personal Writing"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Personal Writing").unsafeRunSync() shouldBe None

    sm.executeCommand(
      Command.typed(
        "save-personal-writing-preset",
        "Save personal writing preset",
        CommandIntent.SaveUiPreset("Personal Writing"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "rename-writing-preset",
        "Rename writing preset",
        CommandIntent.RenameUiPreset("Personal Writing", "Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Personal Writing").unsafeRunSync() shouldBe None
    store.find("Drafting").unsafeRunSync() should not be empty

    sm.executeCommand(
      Command.typed(
        "delete-writing-preset",
        "Delete writing preset",
        CommandIntent.DeleteUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    store.find("Drafting").unsafeRunSync() shouldBe None
    com.serenity.ui.presets.UiPreset.builtIn("Writing") should not be empty
  }

  it should "reject saving Create and Duplicate drafts over an existing preset" in {
    val path     = Files.createTempDirectory("state-manager-ui-preset-draft-collision").resolve("ui-presets.json")
    val store    = UiPresetStore(path)
    val existing = UiPreset("Drafting", AppConfig.default.withLineNumbers(false), Theme.dark.name, Nil)
    val sm       = managerWithStore(store)
    store.upsert(existing).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed("create", "Create", CommandIntent.StartUiPresetDraft("Drafting"), CommandCategory.Settings)
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed("save", "Save", CommandIntent.SaveUiPreset("Drafting"), CommandCategory.Settings)
    ).unsafeRunSync()
    store.find("Drafting").unsafeRunSync() shouldBe Some(existing)
    commandRunnerState(sm).statusMessage.getOrElse(fail("save failure should be visible")) should include(
      "Could not save Drafting"
    )

    sm.executeCommand(Command.typed("discard", "Discard", CommandIntent.DiscardUiPresetDraft, CommandCategory.Settings))
      .unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "duplicate",
        "Duplicate",
        CommandIntent.DuplicateUiPreset("Writing", "Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed("save", "Save", CommandIntent.SaveUiPreset("Drafting"), CommandCategory.Settings)
    ).unsafeRunSync()
    store.find("Drafting").unsafeRunSync() shouldBe Some(existing)
    commandRunnerState(sm).statusMessage.getOrElse(fail("save failure should be visible")) should include(
      "Could not save Drafting"
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
        CommandIntent.RenameUiPreset("Writing", "Personal Writing"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val runner = commandRunnerState(sm)

    store.find("Personal Writing").unsafeRunSync() shouldBe None
    runner.editingPresetName shouldBe Some("Writing")
    runner.statusMessage shouldBe Some("Built-in preset cannot be renamed. Duplicate Writing first.")
  }

  it should "restore a dirty preset draft into a reopened command runner" in {
    val path = Files.createTempDirectory("state-manager-ui-preset-reopen").resolve("ui-presets.json")
    val sm   = managerWithStore(UiPresetStore(path))
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed("create", "Create", CommandIntent.StartUiPresetDraft("Drafting"), CommandCategory.Settings)
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "change",
        "Change",
        CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val runner = commandRunnerState(sm)
    runner.editingPresetName shouldBe Some("Drafting")
    runner.statusMessage shouldBe Some(
      "Preset draft has unsaved changes. Save commits them; Discard restores the workspace."
    )
  }

  it should "edit custom presets as drafts and reject built-in edit requests" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    store.upsert(UiPreset("Drafting", AppConfig.default.withLineNumbers(false), Theme.dark.name, Nil)).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(Command.typed("edit", "Edit", CommandIntent.EditUiPreset("Drafting"), CommandCategory.Settings))
      .unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().uiPresetEditSession.flatMap(_.sourceName) shouldBe Some("Drafting")
    sm.executeCommand(Command.typed("discard", "Discard", CommandIntent.DiscardUiPresetDraft, CommandCategory.Settings))
      .unsafeRunSync()
    sm.executeCommand(Command.typed("edit", "Edit", CommandIntent.EditUiPreset("Writing"), CommandCategory.Settings))
      .unsafeRunSync()
    commandRunnerState(sm).statusMessage shouldBe Some("Built-in preset cannot be edited. Duplicate Writing first.")
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
      Command.typed("edit", "Edit", CommandIntent.EditUiPreset("Missing Theme"), CommandCategory.Settings)
    ).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().config shouldBe initial.config
    sm.getCurrentState.unsafeRunSync().uiPresetEditSession shouldBe None
    commandRunnerState(sm).statusMessage.getOrElse(fail("missing preset error")) should include(
      "Theme 'not-installed' could not be loaded"
    )

    sm.executeCommand(
      Command.typed(
        "duplicate",
        "Duplicate",
        CommandIntent.DuplicateUiPreset("Missing Font", "Copy"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().config shouldBe initial.config
    sm.getCurrentState.unsafeRunSync().uiPresetEditSession shouldBe None
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
        CommandIntent.DeleteUiPreset("Writing"),
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
      CommandIntent.ApplyUiPreset("Drafting")
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
        CommandIntent.SaveUiPreset("Drafting"),
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
      CommandIntent.ApplyUiPreset("Drafting")
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

    def runnerState =
      sm.getCurrentState
        .map(
          _.commandRunnerSurface.flatMap {
            _.content match
              case SurfaceContent.CommandPalette(runner) => Some(runner)
              case _                                     => None
          }
        )
        .unsafeRunSync()
        .getOrElse(fail("command runner should stay open"))

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "duplicate-drafting-preset",
        "Duplicate drafting preset",
        CommandIntent.DuplicateUiPreset("Drafting", "Drafting Copy"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    runnerState.editingPresetName shouldBe Some("Drafting Copy")
    runnerState.statusMessage shouldBe Some("Editing unsaved duplicate of Drafting. Save commits it.")

    sm.executeCommand(
      Command.typed(
        "save-drafting-copy-preset",
        "Save drafting copy preset",
        CommandIntent.SaveUiPreset("Drafting Copy"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "rename-drafting-copy-preset",
        "Rename drafting copy preset",
        CommandIntent.RenameUiPreset("Drafting Copy", "Final Draft"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    runnerState.editingPresetName shouldBe Some("Final Draft")
    runnerState.statusMessage shouldBe Some("Preset renamed. Configure Final Draft.")

    sm.executeCommand(
      Command.typed(
        "delete-final-draft-preset",
        "Delete final draft preset",
        CommandIntent.DeleteUiPreset("Final Draft"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    runnerState.editingPresetName shouldBe None
    runnerState.statusMessage shouldBe Some("Preset deleted.")
  }

  it should "open preset options after creating a UI preset from the command runner" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-create-options").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
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
    val submenu = state.uiSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _) =>
        groupId -> previewOnly
    }

    runner.activeSubmenu.map(_.groupId) shouldBe Some("settings-preset-edit")
    runner.activeSubmenu.flatMap(_.parentGroupId) shouldBe Some("settings-ui-presets")
    runner.editingPresetName shouldBe Some("Drafting")
    runner.statusMessage shouldBe Some("Editing draft from the current workspace. Save commits it.")
    state.focus shouldBe Focus.Surface(SurfaceId("command-runner-submenu"))
    submenu shouldBe Some("settings-preset-edit" -> false)
  }

  it should "keep a created preset draft out of storage and restore its baseline on discard" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-transaction").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.StartUiPresetDraft("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "set-drafting-markdown-default",
        "Set drafting document default",
        CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    store.find("Drafting").unsafeRunSync() shouldBe None
    sm.getCurrentState.unsafeRunSync().config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown

    sm.executeCommand(
      Command.typed(
        "discard-preset-draft",
        "Discard preset draft",
        CommandIntent.DiscardUiPresetDraft,
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val restored = sm.getCurrentState.unsafeRunSync()
    restored.config.defaultDocumentMode shouldBe AppConfig.default.defaultDocumentMode
    restored.uiPresetEditSession shouldBe None
    store.find("Drafting").unsafeRunSync() shouldBe None
  }

  it should "mark config changes as an unsaved copy of the preset currently being edited" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-config").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-markdown-default",
        "Set drafting document default",
        CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "set-drafting-subtle-motion",
        "Set drafting motion",
        CommandIntent.SetMotionPreset(MotionPreset.Subtle),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    state.config.motionPreset shouldBe MotionPreset.Subtle
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark appearance edits as an unsaved copy without changing preset panel snapshots" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-appearance-patch").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val existingPanel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val originalPreset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withBackgroundStyle(BackgroundStyle.Solid),
      themeName = Theme.dark.name,
      pinnedPanels = List(existingPanel),
      targetEditorPaneCount = Some(1)
    )
    store.upsert(originalPreset).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-background",
        "Set drafting background",
        CommandIntent.SetBackgroundStyle(BackgroundStyle.GlassLike),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.config.backgroundStyle shouldBe BackgroundStyle.GlassLike
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark motion edits as an unsaved copy without changing preset panel snapshots" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-motion-patch").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val existingPanel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val originalPreset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withMotionPreset(MotionPreset.Reduced),
      themeName = Theme.dark.name,
      pinnedPanels = List(existingPanel),
      targetEditorPaneCount = Some(1)
    )
    store.upsert(originalPreset).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-motion",
        "Set drafting motion",
        CommandIntent.SetMotionPreset(MotionPreset.Subtle),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.config.motionPreset shouldBe MotionPreset.Subtle
    state.config.characterAnimation shouldBe MotionPreset.Subtle.animationConfig
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark typography edits as an unsaved copy without changing preset panel snapshots" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-typography-patch").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val existingPanel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val originalPreset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.copy(fontConfig = AppConfig.default.fontConfig.copy(textFontSize = 12.0f)),
      themeName = Theme.dark.name,
      pinnedPanels = List(existingPanel),
      targetEditorPaneCount = Some(1)
    )
    store.upsert(originalPreset).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-prose-size",
        "Set drafting prose font size",
        CommandIntent.SetTextFontSize(18.0f),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.config.fontConfig.textFontSize shouldBe 18.0f
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark document default edits as an unsaved copy without changing preset panel snapshots" in {
    val path =
      Files.createTempDirectory("state-manager-ui-preset-edit-document-default-patch").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val existingPanel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val originalPreset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withDefaultDocumentMode(DefaultDocumentMode.PlainText),
      themeName = Theme.dark.name,
      pinnedPanels = List(existingPanel),
      targetEditorPaneCount = Some(1)
    )
    store.upsert(originalPreset).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-rich-text-default",
        "Set drafting document default",
        CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.config.defaultDocumentMode shouldBe DefaultDocumentMode.RichText
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark text display edits as an unsaved copy without changing preset panel snapshots" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-text-display-patch").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val existingPanel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val originalPreset = UiPreset(
      name = "Drafting",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(true)
        .withWordWrap(true)
        .withTextAreaInsets(TextAreaInsets.fromPercent(15.0, 15.0)),
      themeName = Theme.dark.name,
      pinnedPanels = List(existingPanel),
      targetEditorPaneCount = Some(1)
    )
    store.upsert(originalPreset).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-wrap",
        "Set drafting wrap",
        CommandIntent.SetWordWrap(false),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "set-drafting-left-inset",
        "Set drafting left inset",
        CommandIntent.SetTextAreaLeftInset(0.2),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.config.wordWrapEnabled shouldBe false
    state.config.textAreaInsets.left shouldBe 0.2
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark language tool edits as an unsaved copy without changing preset panel snapshots" in {
    val path = Files.createTempDirectory("state-manager-ui-preset-edit-language-tools-patch").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val existingPanel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val originalPreset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withSpellCheck(
        SpellCheckConfig(enabled = false, languages = List("en"), additionalWords = List("serenity"))
      ),
      themeName = Theme.dark.name,
      pinnedPanels = List(existingPanel),
      targetEditorPaneCount = Some(1)
    )
    store.upsert(originalPreset).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-spellcheck",
        "Set drafting spell-check",
        CommandIntent.SetSpellCheckEnabled(true),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "set-drafting-spellcheck-language",
        "Set drafting spell-check language",
        CommandIntent.SetSpellCheckLanguages(List("EN", "fr")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.config.spellCheck.enabled shouldBe true
    state.config.spellCheck.languages shouldBe List("en", "fr")
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark theme changes as an unsaved copy of the preset currently being edited" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-theme").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.updateState(_.copy(theme = Theme.light)).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "toggle-drafting-theme",
        "Toggle drafting theme",
        CommandIntent.ToggleTheme,
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.theme.name shouldBe Theme.dark.name
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark markdown preview mode changes as an unsaved copy of the preset currently being edited" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-markdown").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-markdown-preview",
        "Set drafting Markdown preview",
        CommandIntent.SetMarkdownViewMode(MarkdownViewMode.SplitPreview),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.config.markdownViewMode shouldBe MarkdownViewMode.SplitPreview
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark pinned panel changes as an unsaved copy of the preset currently being edited" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-panels").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "pin-drafting-outline",
        "Pin drafting outline",
        CommandIntent.PinOutlinePanel,
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-drafting-diagnostics",
        "Pin drafting diagnostics",
        CommandIntent.PinDiagnosticsPanel,
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.pinnedSurfaces.collect {
      case UiSurface(_, content, SurfacePresentation.Pinned(position, _), _) => position -> content
    } should contain allOf (
      PanelPosition.Right  -> SurfaceContent.Outline(Nil),
      PanelPosition.Bottom -> SurfaceContent.Diagnostics(Nil)
    )
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "expose panel reorder commands while editing a preset" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-panel-order-menu").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-drafting-outline-right",
        "Pin drafting outline right",
        CommandIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right)),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-drafting-diagnostics-right",
        "Pin drafting diagnostics right",
        CommandIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Right)),
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

    commands should contain("Move Outline Earlier" -> CommandIntent.MovePanelEarlier(PanelKind.Outline))
    commands should contain("Move Outline Later" -> CommandIntent.MovePanelLater(PanelKind.Outline))
    commands should contain("Move Diagnostics Earlier" -> CommandIntent.MovePanelEarlier(PanelKind.Diagnostics))
    commands should contain("Move Diagnostics Later" -> CommandIntent.MovePanelLater(PanelKind.Diagnostics))
  }

  it should "mark reordered active panels as an unsaved copy of the preset currently being edited" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-panel-order").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))
    sm.executeCommand(
      Command.typed(
        "pin-drafting-outline-right",
        "Pin drafting outline right",
        CommandIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right)),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-drafting-diagnostics-right",
        "Pin drafting diagnostics right",
        CommandIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Right)),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "move-drafting-diagnostics-earlier",
        "Move drafting diagnostics earlier",
        CommandIntent.MovePanelEarlier(PanelKind.Diagnostics),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.pinnedSurfaces.collect {
      case UiSurface(_, content, SurfacePresentation.Pinned(PanelPosition.Right, _), _) => content
    } shouldBe List(
      SurfaceContent.Diagnostics(Nil),
      SurfaceContent.Outline(Nil)
    )
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "mark unpinned panel changes as an unsaved copy of the preset currently being edited" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-unpin").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))
    sm.executeCommand(
      Command.typed(
        "pin-drafting-outline",
        "Pin drafting outline",
        CommandIntent.PinOutlinePanel,
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "unpin-drafting-outline",
        "Unpin drafting outline",
        CommandIntent.UnpinPanel(PanelPosition.Right),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    state.pinnedSurfaces.collect {
      case UiSurface(_, _, SurfacePresentation.Pinned(position, _), _) => position
    } should not contain PanelPosition.Right
    saved shouldBe savedBefore
    assertPresetMarkedUnsaved(sm)
  }

  it should "persist an unsaved edited preset only when explicitly saved" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-save-unsaved-copy").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-create",
        "Create preset",
        CommandIntent.SaveUiPreset("Drafting"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-rich-text-default",
        "Set drafting document default",
        CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "save-drafting-copy",
        "Save drafting copy",
        CommandIntent.SaveUiPreset("Drafting Edited"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val original = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))
    val copy     = store.find("Drafting Edited").unsafeRunSync().getOrElse(fail("Drafting Edited preset should exist"))
    val runner   = commandRunnerState(sm)

    original shouldBe savedBefore
    copy.config.defaultDocumentMode shouldBe DefaultDocumentMode.RichText
    sm.getCurrentState.unsafeRunSync().uiPresetEditSession.map(_.draft.config.defaultDocumentMode) shouldBe
      Some(DefaultDocumentMode.RichText)
    runner.editingPresetName shouldBe Some("Drafting Edited")
    runner.statusMessage shouldBe Some("Preset saved. Configure Drafting Edited.")
  }

  it should "restore a dirty preview draft through a session restart and discard it from a reopened runner" in {
    val sessionRoot = Files.createTempDirectory("state-manager-ui-preset-restart")
    val store       = UiPresetStore(sessionRoot.resolve("ui-presets.json"))
    val sm          = managerWithStore(store, sessionRoot = Some(sessionRoot))
    val baseline    = sm.getCurrentState.unsafeRunSync().config.materialPreset

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "create-draft",
        "Create draft",
        CommandIntent.StartUiPresetDraft("Restart Draft"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "preview-solid-material",
        "Preview solid material",
        CommandIntent.SetMaterialPreset(MaterialPreset.Solid),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.saveSession().unsafeRunSync()

    val restarted = managerWithStore(store, sessionRoot = Some(sessionRoot))
    restarted
      .executeCommand(
        Command.typed(
          "restore-session",
          "Restore session",
          CommandIntent.StartupRestoreSession,
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    restarted.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val restored = restarted.getCurrentState.unsafeRunSync()
    restored.config.materialPreset shouldBe MaterialPreset.Solid
    restored.uiPresetEditSession.map(_.draftName) shouldBe Some("Restart Draft")
    restored.uiPresetEditSession.map(_.dirty) shouldBe Some(true)
    store.find("Restart Draft").unsafeRunSync() shouldBe None

    restarted
      .executeCommand(
        Command.typed("discard-draft", "Discard draft", CommandIntent.DiscardUiPresetDraft, CommandCategory.Settings)
      )
      .unsafeRunSync()

    val discarded = restarted.getCurrentState.unsafeRunSync()
    discarded.config.materialPreset shouldBe baseline
    discarded.uiPresetEditSession shouldBe None
    store.find("Restart Draft").unsafeRunSync() shouldBe None
  }

  it should "restore the complete pane layout after discarding a pane-count-changing preview" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-discard-layout").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val baselineLayout = Layout(
      editorPanes = Map(
        PaneId(0) -> EditorPane.withBuffer(PaneId(0), BufferId(0)),
        PaneId(4) -> EditorPane.withBuffer(PaneId(4), BufferId(1))
      ),
      activeEditorPaneId = Some(PaneId(4)),
      paneOrder = List(PaneId(4), PaneId(0)),
      splitDirection = PaneSplitDirection.Horizontal
    )

    sm.updateState { state =>
      state.copy(
        buffers = state.buffers + (BufferId(1) -> Buffer.newEmpty(BufferId(1))),
        bufferOrder = List(BufferId(0), BufferId(1)),
        layout = baselineLayout,
        focus = Focus.EditorPane(PaneId(4)),
        nextBufferId = BufferId(2),
        nextPaneId = PaneId(5)
      )
    }.unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val baselineFocus = sm.getCurrentState.unsafeRunSync().focus
    sm.executeCommand(
      Command.typed(
        "create-layout-draft",
        "Create layout draft",
        CommandIntent.StartUiPresetDraft("Layout Draft"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "preview-writing-layout",
        "Preview writing layout",
        CommandIntent.ApplyUiPreset("Writing"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().layout.editorPanes should have size 1

    sm.executeCommand(
      Command
        .typed("discard-layout-draft", "Discard draft", CommandIntent.DiscardUiPresetDraft, CommandCategory.Settings)
    ).unsafeRunSync()

    val discarded = sm.getCurrentState.unsafeRunSync()
    discarded.layout shouldBe baselineLayout
    discarded.focus shouldBe baselineFocus
    discarded.nextPaneId shouldBe PaneId(5)
  }

  it should "reset a custom built-in preset override to the built-in defaults" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-reset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    sm.executeCommand(
      Command.typed(
        "reset-writing-preset",
        "Reset writing preset",
        CommandIntent.ResetUiPreset("Writing"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "apply-writing-preset",
        "Apply writing preset",
        CommandIntent.ApplyUiPreset("Writing"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    store.find("Writing").unsafeRunSync() shouldBe None
    state.config.fontConfig.textFontFamily shouldBe Font.SERIF
    state.pinnedSurfaces.map(_.presentation) shouldBe List(SurfacePresentation.Pinned(PanelPosition.Left, 28))
  }

  it should "not reset a preset while another preset draft has unsaved changes" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-reset-dirty-draft").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val writingOverride = UiPreset(
      "Writing",
      AppConfig.default.withLineNumbers(false),
      Theme.light.name,
      Nil
    )
    Files.writeString(path, s"""{"presets":[${writingOverride.asJson.noSpaces}]}""")
    val sm = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed("create", "Create", CommandIntent.StartUiPresetDraft("Drafting"), CommandCategory.Settings)
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "change",
        "Change",
        CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed("reset", "Reset", CommandIntent.ResetUiPreset("Writing"), CommandCategory.Settings)
    ).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().uiPresetEditSession.map(_.draftName) shouldBe Some("Drafting")
    store.find("Writing").unsafeRunSync() shouldBe Some(writingOverride)
    commandRunnerState(sm).statusMessage shouldBe Some(
      "Save, Discard, or Cancel the current preset draft before switching presets."
    )
  }

  it should "cancel a blocked preset switch without losing the dirty draft" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-cancel-switch").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed("create", "Create", CommandIntent.StartUiPresetDraft("Drafting"), CommandCategory.Settings)
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "change",
        "Change",
        CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed("apply", "Apply", CommandIntent.ApplyUiPreset("Writing"), CommandCategory.Settings)
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed("cancel", "Cancel", CommandIntent.CancelUiPresetSwitch, CommandCategory.Settings)
    ).unsafeRunSync()

    val state  = sm.getCurrentState.unsafeRunSync()
    val runner = commandRunnerState(sm)

    state.uiPresetEditSession.map(_.draftName) shouldBe Some("Drafting")
    state.uiPresetEditSession.map(_.dirty) shouldBe Some(true)
    state.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    runner.settingsGroups.flatMap(descendants).collect {
      case input: CommandSurfaceItem.InputItem => input.id
    } should contain("ui-preset-cancel-switch")
    runner.statusMessage shouldBe Some("Preset switch cancelled. Continue editing Drafting.")
  }
