package com.serenity

import java.awt.Font

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.{AppMode, ToolbarDisplayMode}
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How the toolbar's item set and glyphs are chosen and rendered: which items appear for the active buffer/app mode,
  * how the icon-only/text-only/icon-and-text display mode renders and reacts to preference changes, semantic
  * formatting-group separators, and the bundled Material Icons Round glyph coverage. Placement/positioning is
  * covered in [[ContextualToolbarPlacementSpec]], keyboard-driven focus and detail lifecycle in
  * [[ContextualToolbarDetailSpec]], and mouse interaction in [[ContextualToolbarMouseSpec]].
  */
class ContextualToolbarDisplaySpec extends AnyFlatSpec with Matchers with ContextualToolbarTestSupport:

  "Contextual toolbar" should "separate paragraph-role and alignment controls into their own compact groups" in {
    val stateManager = createStateManager("ContextualToolbarSpec-semantic-groups")

    seedToolbarDocument(stateManager)

    val items              = ContextualToolbar.itemsFor(stateManager.getCurrentState.unsafeRunSync())
    val paragraphRoleIndex = items.indexWhere(_.id == "paragraph-role")
    val paragraphRole      = items.lift(paragraphRoleIndex).getOrElse(fail("Expected paragraph role control"))
    val alignment          = items.lift(paragraphRoleIndex + 1).getOrElse(fail("Expected alignment control"))

    ContextualToolbar.hasTrailingGroupSeparator(paragraphRole, Some(alignment)) shouldBe true
  }

  it should "open with the configured display mode and refresh when the preference changes" in {
    val stateManager = createStateManager("ContextualToolbarSpec-display-mode")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).displayMode shouldBe ToolbarDisplayMode.TextOnly

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "contextual-toolbar-icon-only",
          "Set contextual toolbar display to icon only",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.config.surfaceConfig.contextualToolbarDisplayMode shouldBe ToolbarDisplayMode.IconOnly
    toolbarStateFrom(state).displayMode shouldBe ToolbarDisplayMode.IconOnly
  }

  it should "render dropdowns and inputs according to the toolbar display mode" in {
    val dropdown = ContextualToolbarItem.Dropdown(
      id = "font-family",
      label = "Font",
      icon = "",
      optionItem = CommandSurfaceItem.OptionItem(
        id = "font-family",
        label = "Font",
        options = List(
          com.serenity.command
            .CommandOption("Serif", CommandIntent.RichText(RichTextIntent.SetRichTextFontFamily("Serif")))
        ),
        selectedIndex = 0,
        category = CommandCategory.Edit
      )
    )
    val input = ContextualToolbarItem.Input(
      id = "font-size",
      label = "Size",
      icon = "",
      inputItem = CommandSurfaceItem.InputItem(
        id = "font-size",
        label = "Size",
        hint = "Points",
        currentValue = "18",
        kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
        parse = _.toFloatOption.map(commandIntentArg =>
          CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(commandIntentArg))
        ),
        category = CommandCategory.Edit
      )
    )

    ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.IconOnly) shouldBe ""
    ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.TextOnly) shouldBe "Font Serif"
    ContextualToolbar.displayText(dropdown, ToolbarDisplayMode.IconAndText) shouldBe " Font Serif"

    ContextualToolbar.displayText(input, ToolbarDisplayMode.IconOnly) shouldBe ""
    ContextualToolbar.displayText(input, ToolbarDisplayMode.TextOnly) shouldBe "Size 18"
    ContextualToolbar.displayText(input, ToolbarDisplayMode.IconAndText) shouldBe " Size 18"
  }

  it should "not offer build/test/run/debug buttons for a code buffer while the app is in prose mode" in {
    val stateManager = createStateManager("ContextualToolbarSpec-prose-mode")
    stateManager
      .updateState { state =>
        val bufferId = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val buffer   = state.persisted.buffers(bufferId)
        val updated  = buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Scala)))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, updated)))
      }
      .unsafeRunSync()

    val codeModeItems = ContextualToolbar.itemsFor(stateManager.getCurrentState.unsafeRunSync())
    codeModeItems shouldBe ContextualToolbar.codeItems

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "app-mode-prose",
          "Switch to prose mode",
          CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val proseModeItems = ContextualToolbar.itemsFor(stateManager.getCurrentState.unsafeRunSync())
    proseModeItems.map(_.id) should not contain "project-run"
    proseModeItems should not be ContextualToolbar.codeItems
  }

  it should "use Material Icons Round code points in icon-only mode" in {
    ContextualToolbar.markdownItems.map(_.icon) shouldBe List("", "", "", "")
    ContextualToolbar.codeItems.map(_.icon) shouldBe List("", "", "", "")
    ContextualToolbar.codeItems.map(_.label) shouldBe List("Build", "Test", "Run", "Run Debug Task")

    val stateManager = createStateManager("ContextualToolbarSpec-glyphs")
    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val icons =
      ContextualToolbar
        .itemsFor(stateManager.getCurrentState.unsafeRunSync())
        .map(item => item.id -> item.icon)
        .toMap

    icons shouldBe Map(
      "bold"             -> "",
      "italic"           -> "",
      "underline"        -> "",
      "font-family"      -> "",
      "font-family-text" -> "",
      "font-size"        -> "",
      "color"            -> "",
      "color-hex"        -> "",
      "paragraph-role"   -> "",
      "align-left"       -> "",
      "align-center"     -> "",
      "align-right"      -> "",
      "align-justify"    -> ""
    )
  }

  it should "render every compact toolbar control as an icon-only glyph" in {
    val stateManager = createStateManager("ContextualToolbarSpec-rendered-glyphs")
    val viewport     = ViewportSize(120, 30)
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state   = stateManager.getCurrentState.unsafeRunSync()
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new MockRenderSurface(viewport.width, viewport.height)

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      font,
      font,
      CellMetrics.fromFont(font),
      None
    )

    val renderedText = surface.putStringCalls.map(_.s).mkString
    renderedText should include("│")
    ContextualToolbar.itemsFor(state).map(_.icon).foreach(renderedText should include(_))
    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      toolbarStateFrom(state),
      state,
      LayoutRect(0, 0, 120, 10),
      SurfaceRenderMode.Floating
    )
    resolved.rows.flatMap(_.segments).map(_.text) shouldBe ContextualToolbar.itemsFor(state).map(_.icon)
    surface.setFontCalls.map(_.getFamily) should contain(FontLoader.ToolbarIconFontFamily)
  }

  it should "visually separate semantic formatting control groups" in {
    val stateManager = createStateManager("ContextualToolbarSpec-group-separators")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val state = stateManager.getCurrentState.unsafeRunSync()
    val resolved = SurfaceContentResolver.resolveContextualToolbar(
      ContextualToolbarState(displayMode = ToolbarDisplayMode.IconOnly),
      state,
      LayoutRect(0, 0, 120, 10),
      SurfaceRenderMode.Floating
    )

    resolved.rows.head.segments.filter(_.trailingSeparator).map(_.text) shouldBe List(
      ContextualToolbar.displayText(toolbarButton(state, "underline"), ToolbarDisplayMode.IconOnly),
      ContextualToolbar.displayText(toolbarInput(state, "font-size"), ToolbarDisplayMode.IconOnly),
      ContextualToolbar.displayText(toolbarInput(state, "color-hex"), ToolbarDisplayMode.IconOnly),
      ContextualToolbar.displayText(toolbarDropdown(state, "paragraph-role"), ToolbarDisplayMode.IconOnly)
    )
  }

  it should "render icon-font glyphs alongside labels in IconAndText mode" in {
    val stateManager = createStateManager("ContextualToolbarSpec-rendered-icon-and-text")
    val viewport     = ViewportSize(120, 30)
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconAndText))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state   = stateManager.getCurrentState.unsafeRunSync()
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new MockRenderSurface(viewport.width, viewport.height)

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      font,
      font,
      CellMetrics.fromFont(font),
      None
    )

    val renderedText = surface.putStringCalls.map(_.s).mkString
    ContextualToolbar.itemsFor(state).foreach { item =>
      renderedText should include(item.icon)
      renderedText should include(ContextualToolbar.displayText(item, ToolbarDisplayMode.TextOnly))
    }
    surface.setFontCalls.map(_.getFamily) should contain(
      FontLoader.toolbarIconFontFamily.getOrElse(fail("Expected bundled toolbar icon font"))
    )
  }

  it should "use toolbar glyphs supported by the bundled Material Icons Round font" in {
    val stateManager = createStateManager("ContextualToolbarSpec-font-coverage")
    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val glyphs =
      ContextualToolbar.markdownItems.map(_.icon) ++
        ContextualToolbar.codeItems.map(_.icon) ++
        ContextualToolbar.itemsFor(stateManager.getCurrentState.unsafeRunSync()).map(_.icon)
    val font = FontLoader.toolbarIconFont(24.0f).getOrElse(fail("Expected bundled Material Icons Round font"))

    glyphs.foreach { glyph =>
      withClue(s"Font '${font.getFontName}' cannot display toolbar glyph '$glyph': ") {
        font.canDisplayUpTo(glyph) shouldBe -1
      }
    }
  }

  it should "preserve a selected hex value at the exact compact toolbar width" in {
    val stateManager = createStateManager("ContextualToolbarSpec-compact-selected-hex")
    val viewport     = ViewportSize(78, 30)

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    moveToolbarFocusTo(stateManager, "color-hex")

    val state = stateManager.getCurrentState.unsafeRunSync()
    toolbarContentWidth(state) shouldBe 55

    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val surface = new MockRenderSurface(viewport.width, viewport.height)
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      font,
      font,
      CellMetrics.fromFont(font),
      None
    )

    val row        = surface.getRow(toolbarRowY(state, 0))
    val hexStart   = row.indexOf(toolbarInput(state, "color-hex").icon)
    val separatorX = row.indexOf('│')
    hexStart should be >= 0
    separatorX should be >= 0
    surface.getBg(hexStart, toolbarRowY(state, 0)) shouldBe state.persisted.theme.highlighted.background
    surface.getBg(separatorX, toolbarRowY(state, 0)) shouldBe state.persisted.theme.panel.background
  }
