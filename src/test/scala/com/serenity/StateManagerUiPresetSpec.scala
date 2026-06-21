package com.serenity

import java.awt.Font
import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{Command, CommandCategory, CommandIntent}
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
    onWindowSizeChanged: PreferredWindowSize => IO[Unit] = _ => IO.unit
  ): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerUiPresetSpec"))
    StateManager
      .apply(
        logger,
        uiPresetStore = store,
        windowSizeProvider = windowSize,
        onPreferredWindowSizeChanged = onWindowSizeChanged
      )
      .unsafeRunSync()

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

  it should "apply a named preset and notify runtime window size changes" in {
    val path               = Files.createTempDirectory("state-manager-ui-preset-apply").resolve("ui-presets.json")
    val store              = UiPresetStore(path)
    val observedWindowSize = Ref.of[IO, Option[PreferredWindowSize]](None).unsafeRunSync()
    val sm = managerWithStore(
      store,
      onWindowSizeChanged = size => observedWindowSize.set(Some(size))
    )
    val preset = com.serenity.ui.presets.UiPreset(
      name = "Review",
      config = AppConfig.default.copy(
        backgroundStyle = BackgroundStyle.Solid,
        preferredWindowSize = Some(PreferredWindowSize(1280, 720))
      ),
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

    sm.executeCommand(
      Command.typed(
        "apply-review-preset",
        "Apply review preset",
        CommandIntent.ApplyUiPreset("Review"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.config.backgroundStyle shouldBe BackgroundStyle.Solid
    state.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1280, 720))
    state.theme.name shouldBe Theme.dark.name
    state.pinnedSurfaces.map(_.presentation) shouldBe List(SurfacePresentation.Pinned(PanelPosition.Right, 36))
    observedWindowSize.get.unsafeRunSync() shouldBe Some(PreferredWindowSize(1280, 720))
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
    store.find("Personal Writing").unsafeRunSync() should not be empty

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

  it should "keep built-in presets immutable for delete commands" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-delete-built-in").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val customWriting = UiPreset(
      name = "Writing",
      config = AppConfig.default.copy(backgroundStyle = BackgroundStyle.Solid),
      themeName = Theme.dark.name,
      pinnedPanels = Nil
    )
    store.upsert(customWriting).unsafeRunSync()

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

    store.find("Writing").unsafeRunSync() should not be empty
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
    val customPreset = presetGroup.children
      .collectFirst {
        case item: com.serenity.command.CommandSurfaceItem.OptionItem if item.id == "ui-preset-custom" => item
      }
      .getOrElse(fail("missing custom preset picker"))

    customPreset.options.map(_.label) shouldBe List("Drafting")
    customPreset.options.headOption.map(_.intent) shouldBe Some(CommandIntent.ApplyUiPreset("Drafting"))
    customPreset.options.headOption.flatMap(_.hint) shouldBe Some(
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
    val customPreset = presetGroup.children
      .collectFirst {
        case item: com.serenity.command.CommandSurfaceItem.OptionItem if item.id == "ui-preset-custom" => item
      }
      .getOrElse(fail("missing custom preset picker"))

    customPreset.options.map(_.label) shouldBe List("Drafting")
    customPreset.options.headOption.map(_.intent) shouldBe Some(CommandIntent.ApplyUiPreset("Drafting"))
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
    runnerState.statusMessage shouldBe Some("Preset duplicated. Configure Drafting Copy.")

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

    runner.activeSubmenu.map(_.groupId) shouldBe Some("ui-preset-configure")
    runner.activeSubmenu.flatMap(_.parentGroupId) shouldBe Some("settings-ui-presets")
    runner.editingPresetName shouldBe Some("Drafting")
    runner.statusMessage shouldBe Some("Preset saved. Configure workspace options.")
    state.focus shouldBe Focus.Surface(SurfaceId("command-runner-submenu"))
    submenu shouldBe Some("ui-preset-configure" -> false)
  }

  it should "persist config changes to the preset currently being edited" in {
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

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    saved.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    saved.config.motionPreset shouldBe MotionPreset.Subtle
  }

  it should "persist markdown preview mode changes to the preset currently being edited" in {
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

    sm.executeCommand(
      Command.typed(
        "set-drafting-markdown-preview",
        "Set drafting Markdown preview",
        CommandIntent.SetMarkdownViewMode(MarkdownViewMode.SplitPreview),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    saved.config.markdownViewMode shouldBe MarkdownViewMode.SplitPreview
  }

  it should "persist pinned panel changes to the preset currently being edited" in {
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

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    saved.pinnedPanels.map(panel => panel.position -> panel.content) should contain allOf (
      PanelPosition.Right  -> UiPreset.PanelContentSnapshot.Outline(Nil),
      PanelPosition.Bottom -> UiPreset.PanelContentSnapshot.Diagnostics(Nil)
    )
  }

  it should "persist unpinned panel changes to the preset currently being edited" in {
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

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    saved.pinnedPanels.map(_.position) should not contain PanelPosition.Right
  }

  it should "reset a custom built-in preset override to the built-in defaults" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-reset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val customWriting = UiPreset(
      name = "Writing",
      config = AppConfig.default.copy(backgroundStyle = BackgroundStyle.Solid),
      themeName = Theme.dark.name,
      pinnedPanels = Nil
    )
    store.upsert(customWriting).unsafeRunSync()

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
