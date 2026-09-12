package com.serenity

import java.awt.Font
import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerUiPresetApplySpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def managerWithStore(
    store: UiPresetStore,
    windowSize: IO[Option[PreferredWindowSize]] = IO.pure(None),
    onWindowSizeChanged: PreferredWindowSize => IO[Unit] = _ => IO.unit,
    sessionRoot: Option[Path] = None
  ): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerUiPresetApplySpec"))
    StateManager
      .apply(
        logger,
        uiPresetStore = store,
        windowSizeProvider = windowSize,
        onPreferredWindowSizeChanged = onWindowSizeChanged,
        sessionRootOverride = sessionRoot
      )
      .unsafeRunSync()

  "StateManager UI presets" should "save the current UI preset to the preset store" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-save").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val size  = PreferredWindowSize(1500, 950)
    val sm    = managerWithStore(store, IO.pure(Some(size)))

    sm.panelManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 12).unsafeRunSync()
    sm.updateState(state =>
      state.copy(
        persisted = state.persisted.copy(
          config = state.persisted.config.withBackgroundStyle(BackgroundStyle.GlassLike),
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
    saved.map(_.config.surfaceConfig.backgroundStyle) shouldBe Some(BackgroundStyle.GlassLike)
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
      dockedPanels = List(
        com.serenity.ui.layout.SessionDockedPanel(
          "panel-1",
          com.serenity.ui.layout.SessionPinnedPanel
            .fromPanelContent(
              PanelContent.Outline(Nil),
              PanelPosition.Right,
              36
            )
            .getOrElse(fail("outline should be capturable"))
        )
      )
    )
    store.upsert(preset).unsafeRunSync()
    sm.paneManager.handleViewportResize(ViewportSize(90, 28)).unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-review-preset",
        "Apply review preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Review Custom")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.persisted.config.surfaceConfig.backgroundStyle shouldBe BackgroundStyle.Solid
    state.persisted.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1280, 720))
    state.runtime.viewportSize shouldBe Some(ViewportSize(90, 28))
    state.persisted.theme.name shouldBe Theme.dark.name
    state.pinnedSurfaces.map(_.presentation) shouldBe List(SurfacePresentation.Docked)
    state.pinnedSurfaces.map(surface =>
      state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id))
    ) shouldBe List(Some(PanelPosition.Right))
    state.pinnedSurfaces.map(surface =>
      com.serenity.state.reducers.PanelStateReducer.currentSize(surface.id, state)
    ) shouldBe List(Some(36))
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
            workspaceTree = Some(TestWorkspaceTrees.linear(pane0, pane1))
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

    state.persisted.config.editorConfig.fontConfig.textFontFamily shouldBe Font.SERIF
    state.persisted.config.surfaceConfig.showLineNumbers shouldBe false
    state.persisted.config.surfaceConfig.showGutter shouldBe false
    state.persisted.layout.editorPanes should have size 1
    state.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(1))
    state.persisted.layout.editorPanes(PaneId(1)).bufferId shouldBe Some(BufferId(1))
    state.persisted.buffers(BufferId(1)).richText.richTextDocument should not be empty
    state.persisted.config.surfaceConfig.showPaneHeaders shouldBe false
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
    config.surfaceConfig.showLineNumbers shouldBe false
    config.surfaceConfig.showPaneHeaders shouldBe false
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
      case surface @ UiSurface(_, SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"), _, _)
          if state.persisted.layout.workspaceTree
            .flatMap(_.positionForSurface(surface.id))
            .contains(
              PanelPosition.Right
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
      case surface @ UiSurface(_, SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"), _, _)
          if state.persisted.layout.workspaceTree
            .flatMap(_.positionForSurface(surface.id))
            .contains(
              PanelPosition.Right
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
      case surface @ UiSurface(_, SurfaceContent.Outline(symbols, _), _, _)
          if state.persisted.layout.workspaceTree
            .flatMap(_.positionForSurface(surface.id))
            .contains(
              PanelPosition.Left
            ) =>
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
