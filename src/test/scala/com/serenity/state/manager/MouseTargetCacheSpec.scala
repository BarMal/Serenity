package com.serenity.state.manager

import java.awt.Font

import com.serenity.command.{Command, CommandPaletteState, CommandRegistry, CommandRunner, CommandRunnerSurface}
import com.serenity.config.{AppConfig, InterfaceDensity, TextAreaInsets}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MouseTargetCacheSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(buffer: Buffer, config: AppConfig = AppConfig.default): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      )
    )

  "MouseTargetLayoutKey" should "ignore cursor and selection changes during mouse drags" in {
    val plainBuffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma")
    val buffer      = plainBuffer.copy(editing = plainBuffer.editing.copy(cursors = List(CursorPosition(0, 1))))
    val state       = stateWith(buffer)
    val draggedState = state.copy(persisted =
      state.persisted.copy(
        buffers = state.persisted.buffers.updated(
          bufferId,
          buffer.copy(
            editing = buffer.editing.copy(
              cursors = List(CursorPosition(1, 3)),
              selection = Some(Selection(CursorPosition(0, 1), CursorPosition(1, 3)))
            )
          )
        )
      )
    )

    MouseTargetLayoutKey.from(state, ViewportSize(80, 24)) shouldBe
      MouseTargetLayoutKey.from(draggedState, ViewportSize(80, 24))
  }

  it should "reuse the same key instance without rewalking panes/buffers/surfaces when nothing layout-relevant changed" in {
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma")
    val state  = stateWith(buffer)
    // Touches only a field MouseTargetLayoutKey.from never reads, so layout/buffers/uiSurfaces/config/focus
    // all stay reference-identical to the previous call.
    val unrelatedChange = state.copy(runtime = state.runtime.copy(clipboard = Some("copied text")))

    val first  = MouseTargetLayoutKey.from(state, ViewportSize(80, 24))
    val second = MouseTargetLayoutKey.from(unrelatedChange, ViewportSize(80, 24))

    second should be theSameInstanceAs first
  }

  it should "cache full editor pane layouts for mouse hit testing" in {
    val config = AppConfig.default.withTextAreaInsets(TextAreaInsets(0.15, 0.10))
    val state  = stateWith(Buffer.fromString(bufferId, "alpha\nbeta"), config)
    val size   = ViewportSize(80, 24)
    val cache  = MouseTargetCache.fromState(state, size)
    val layout = LayoutEngine.calculateLayoutWithUI(state, size)

    cache.scene.paneLayouts shouldBe LayoutEngine.calculateEditorPaneLayouts(state, layout)
    cache.scene.paneLayouts(paneId).headerRect.bottom.shouldBe(cache.scene.paneLayouts(paneId).contentRect.y)
  }

  it should "cache the authoritative scene used for mouse-target geometry" in {
    val state  = stateWith(Buffer.fromString(bufferId, "alpha\nbeta"))
    val size   = ViewportSize(80, 24)
    val cache  = MouseTargetCache.fromState(state, size)
    val reused = MouseTargetCache.fromState(state, size)
    val layout = LayoutEngine.calculateLayoutWithUI(state, size)

    cache.scene.editorContract.workspace.paneLayouts shouldBe cache.scene.paneLayouts
    cache.scene.calculatedLayout shouldBe layout
    reused.scene should be theSameInstanceAs cache.scene
  }

  it should "reuse the prepared scene for cursor-only state changes" in {
    val buffer = Buffer.fromString(bufferId, "alpha beta")
    val state  = stateWith(buffer.copy(editing = buffer.editing.copy(cursors = List(CursorPosition(0, 1)))))
    val moved  = stateWith(buffer.copy(editing = buffer.editing.copy(cursors = List(CursorPosition(0, 5)))))
    val size   = ViewportSize(80, 24)
    val scene  = MouseTargetCache.fromState(state, size).scene

    MouseTargetCache.fromState(moved, size).scene should be theSameInstanceAs scene
  }

  it should "use the renderer's proportional wrapped snapshot for hit testing" in {
    val state = stateWith(Buffer.fromString(bufferId, (1 to 20).map(_ => "proportional").mkString(" ")))
    val size  = ViewportSize(80, 24)
    val mono  = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val text =
      com.serenity.ui.fonts.FontLoader
        .previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Prose)
    val surface = new com.serenity.MockRenderSurface(size.width, size.height)

    Renderer.render(state, cursorVisible = true, surface, size, mono, text, CellMetrics.fromFont(mono), None)

    val cache    = MouseTargetCache.fromState(state, size)
    val snapshot = cache.scene.textSnapshot(paneId).getOrElse(fail("expected prepared text snapshot"))

    snapshot.usesMeasuredLayout shouldBe true
    snapshot.isProportional shouldBe true
    snapshot.visualLines.size should be > 1
    cache.scene should be theSameInstanceAs MouseTargetCache.fromState(state, size).scene
  }

  it should "wrap a prose pane at the pane's width on the render grid" in {
    val state = stateWith(Buffer.fromString(bufferId, "abcdefghij" * 12))
    val size  = ViewportSize(80, 24)
    val cache = MouseTargetCache.fromState(state, size)
    val codeFont =
      com.serenity.ui.fonts.FontLoader
        .previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Code)
    val textFont =
      com.serenity.ui.fonts.FontLoader
        .previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Prose)
    val snapshot    = cache.scene.textSnapshot(paneId).getOrElse(fail("expected prepared text snapshot"))
    val contentRect = cache.scene.paneLayouts(paneId).contentRect

    CellMetrics.fromFont(textFont).charWidth should not be CellMetrics.fromFont(codeFont).charWidth
    snapshot.panelWidthPx shouldBe contentRect.width * CellMetrics.fromFont(codeFont).charWidth
    all(snapshot.visualLines.map(_.widthPx)) should be <= snapshot.panelWidthPx.toFloat
  }

  it should "give rendering the scene prepared by mouse targeting first" in {
    val state = stateWith(Buffer.fromString(bufferId, "alpha beta"))
    val size  = ViewportSize(80, 24)
    val scene = MouseTargetCache.fromState(state, size).scene
    val codeFont =
      com.serenity.ui.fonts.FontLoader
        .previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Code)
    val textFont =
      com.serenity.ui.fonts.FontLoader
        .previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Prose)
    val surface = new com.serenity.MockRenderSurface(size.width, size.height)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      size,
      codeFont,
      textFont,
      CellMetrics.fromFont(codeFont),
      None
    )

    MouseTargetCache.fromState(state, size).scene should be theSameInstanceAs scene
  }

  it should "share a scene when rendering uses an effective theme copy" in {
    val baseState = stateWith(Buffer.fromString(bufferId, "alpha beta"))
    val state = baseState.copy(runtime =
      baseState.runtime.copy(themeTransition =
        Some(ThemeTransition(com.serenity.ui.theme.Theme.light, currentStep = 1, totalSteps = 4))
      )
    )
    val size = ViewportSize(80, 24)
    val codeFont =
      com.serenity.ui.fonts.FontLoader
        .previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Code)
    val textFont =
      com.serenity.ui.fonts.FontLoader
        .previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Prose)
    val surface = new com.serenity.MockRenderSurface(size.width, size.height)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      size,
      codeFont,
      textFont,
      CellMetrics.fromFont(codeFont),
      None
    )
    val renderedScene = MouseTargetCache.fromState(state, size).scene

    MouseTargetCache
      .fromState(
        state.copy(persisted = state.persisted.copy(theme = com.serenity.ui.theme.Theme.dark)),
        size
      )
      .scene should
      be theSameInstanceAs renderedScene
  }

  it should "change when layout-affecting content changes with line numbers enabled" in {
    val shortState = stateWith(Buffer.fromString(bufferId, "one"))
    val longState  = stateWith(Buffer.fromString(bufferId, (1 to 100).map(i => s"line $i").mkString("\n")))

    MouseTargetLayoutKey.from(shortState, ViewportSize(80, 24)) should not be
      MouseTargetLayoutKey.from(longState, ViewportSize(80, 24))
  }

  it should "invalidate prepared snapshots when font, typography, language, viewport, or rich text changes" in {
    val plainBuffer = Buffer.fromString(bufferId, "alpha beta")
    val buffer      = plainBuffer.copy(document = plainBuffer.document.copy(language = Some(LanguageId.Scala)))
    val state       = stateWith(buffer)
    val size        = ViewportSize(80, 24)
    val key         = MouseTargetLayoutKey.from(state, size)

    val fontChanged =
      stateWith(
        buffer,
        state.persisted.config.withFontConfig(state.persisted.config.editorConfig.fontConfig.copy(fontSize = 14.0f))
      )
    val languageChanged = stateWith(buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Markdown))))
    val languageRemoved = stateWith(buffer.copy(document = buffer.document.copy(language = None)))
    val viewportChanged = stateWith(buffer.copy(viewport = buffer.viewport.copy(topVisualLine = 1)))
    val richTextChanged = stateWith(
      buffer.copy(richText =
        buffer.richText.copy(richTextDocument =
          Some(com.serenity.richtext.RichTextDocument.fromPlainText("alpha beta"))
        )
      )
    )

    List(fontChanged, languageChanged, languageRemoved, viewportChanged, richTextChanged).foreach { changed =>
      MouseTargetLayoutKey.from(changed, size) should not be key
    }
  }

  it should "invalidate scene geometry when text-area insets change" in {
    val state = stateWith(Buffer.fromString(bufferId, "alpha beta"))
    val size  = ViewportSize(80, 24)

    val insetState = state.copy(persisted =
      state.persisted.copy(config = state.persisted.config.withTextAreaInsets(TextAreaInsets(0.2, 0.1, 0.1, 0.1)))
    )

    MouseTargetLayoutKey.from(insetState, size) should not be MouseTargetLayoutKey.from(state, size)
    MouseTargetCache.fromState(insetState, size).scene should not be theSameInstanceAs(
      MouseTargetCache.fromState(state, size).scene
    )
  }

  it should "invalidate scene composition when interface density changes" in {
    val state = stateWith(Buffer.fromString(bufferId, "alpha beta"))
    val size  = ViewportSize(80, 24)

    val spaciousState = state.copy(persisted =
      state.persisted.copy(config = state.persisted.config.withInterfaceDensity(InterfaceDensity.Spacious))
    )

    MouseTargetLayoutKey.from(spaciousState, size) should not be MouseTargetLayoutKey.from(state, size)
    MouseTargetCache.fromState(spaciousState, size).scene should not be theSameInstanceAs(
      MouseTargetCache.fromState(state, size).scene
    )
  }

  private def stateWithStartPage(selectedIndex: Int): AppState =
    val newSessionCommand = Command.typed(
      "startup.new-session",
      "Start a new session",
      com.serenity.command.CommandIntent.Session(com.serenity.command.SessionIntent.StartupNewSession)
    )
    val openFileCommand = Command.typed(
      "startup.open-file",
      "Open an existing file or directory",
      com.serenity.command.CommandIntent.Session(com.serenity.command.SessionIntent.StartupOpenFile)
    )
    val actions = List(
      StartupAction("new-session", "Start a new session", newSessionCommand),
      StartupAction("open-file", "Open a file", openFileCommand)
    )
    val page = StartupPage(title = "Welcome", actions = actions, selectedIndex = selectedIndex)
    val surface = UiSurface(
      SurfaceId("surface-0"),
      SurfaceContent.StartPage(page),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState
      .empty(AppConfig.default)
      .copy(
        persisted = AppState.empty(AppConfig.default).persisted.copy(focus = Focus.Surface(surface.id)),
        runtime = AppState.empty(AppConfig.default).runtime.copy(uiSurfaces = List(surface))
      )

  it should "reuse the prepared scene when only the startup page's selected action changes" in {
    // #892: LayoutEngine.calculateFloatingSurfaceHeight/Width never read StartupPage content (height is
    // unconditionally maxHeight, width is content-independent), so selectedIndex must not defeat this cache --
    // otherwise every arrow-key press on the startup screen forces a full LayoutEngine.calculateLayoutWithUI
    // rebuild on the next mouse-hit-testing call, reintroducing the per-navigation stutter #932 already fixed
    // for the command palette's search text.
    val size     = ViewportSize(80, 24)
    val selected = stateWithStartPage(selectedIndex = 0)
    val moved    = stateWithStartPage(selectedIndex = 1)

    MouseTargetLayoutKey.from(selected, size) shouldBe MouseTargetLayoutKey.from(moved, size)
    MouseTargetCache.fromState(moved, size).scene should be theSameInstanceAs
      MouseTargetCache.fromState(selected, size).scene
  }

  private val commandPaletteBaseState =
    stateWith(Buffer.fromString(bufferId, "alpha beta"))

  private def stateWithCommandPalette(searchTerm: String, selectedIndex: Int = 0): AppState =
    val registry = CommandRegistry.default
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .copy(surface =
        CommandRunnerSurface.Palette(CommandPaletteState(searchTerm = searchTerm, selectedIndex = selectedIndex))
      )
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    commandPaletteBaseState.copy(
      persisted = commandPaletteBaseState.persisted.copy(focus = Focus.Surface(surface.id)),
      runtime = commandPaletteBaseState.runtime.copy(uiSurfaces = List(surface))
    )

  it should "reuse the prepared scene when only the command palette's search text changes" in {
    val size   = ViewportSize(80, 24)
    val typing = stateWithCommandPalette(searchTerm = "b")
    val more   = stateWithCommandPalette(searchTerm = "bl")

    MouseTargetLayoutKey.from(typing, size) shouldBe MouseTargetLayoutKey.from(more, size)
    MouseTargetCache.fromState(more, size).scene should be theSameInstanceAs
      MouseTargetCache.fromState(typing, size).scene
  }

  it should "reuse the prepared scene when only the command palette's selected row changes" in {
    val size     = ViewportSize(80, 24)
    val selected = stateWithCommandPalette(searchTerm = "", selectedIndex = 0)
    val moved    = stateWithCommandPalette(searchTerm = "", selectedIndex = 1)

    MouseTargetLayoutKey.from(selected, size) shouldBe MouseTargetLayoutKey.from(moved, size)
    MouseTargetCache.fromState(moved, size).scene should be theSameInstanceAs
      MouseTargetCache.fromState(selected, size).scene
  }

  // issue #1059: a drilled-in settings group now renders on the one `CommandPalette` surface, whose
  // `SurfaceGeometryKey` was already item-count-agnostic before this migration (see the "Verified against every
  // LayoutEngine consumer..." note on `SurfaceGeometryKey.from`'s `CommandPalette` case) -- there is no second
  // surface whose geometry cache key depends on submenu item count anymore, so the scene-reuse/invalidation
  // scenarios this used to cover no longer apply; "reuse the prepared scene when only the command palette's
  // selected row changes" above already exercises that same item-count-agnostic caching for the one surface.
