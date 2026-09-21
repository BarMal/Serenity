package com.serenity

import java.awt.Font
import java.nio.file.Files

import com.serenity.animation.TransitionKind
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiPresetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "UiPreset" should "capture config, theme, preferred window size, and pinned panels from app state" in {
    val root = Files.createTempDirectory("ui-preset-root")
    val config = AppConfig.default
      .withFontConfig(FontConfig(codeFontFamily = "Monospaced", fontSize = 18.0f))
      .withBackgroundStyle(BackgroundStyle.GlassLike)
    val baseState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(config = config, theme = Theme.light)
    )
    val state = DockedPanelFixtures.dock(
      baseState,
      SurfaceId("panel-1"),
      PanelContent.DirectoryTree(DirectoryTreeData(root), selectedPath = Some(root)),
      PanelPosition.Left,
      32
    )

    val preset = UiPreset.capture("Writing", state, Some(PreferredWindowSize(1440, 960)))

    preset.name shouldBe "Writing"
    preset.config.editorConfig.fontConfig.codeFontFamily shouldBe "Monospaced"
    preset.config.editorConfig.fontConfig.codeFontSize shouldBe 18.0f
    preset.config.surfaceConfig.backgroundStyle shouldBe BackgroundStyle.GlassLike
    preset.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1440, 960))
    preset.themeName shouldBe Theme.light.name
    preset.pinnedPanels.map(panel => panel.position -> panel.size) shouldBe List(PanelPosition.Left -> 32)
  }

  it should "restore captured config, theme, and pinned panels onto app state" in {
    val root = Files.createTempDirectory("ui-preset-restore")
    val initial = DockedPanelFixtures.dock(
      AppState.initial,
      SurfaceId("old-panel"),
      PanelContent.Diagnostics(Nil),
      PanelPosition.Bottom,
      8
    )
    val preset = UiPreset(
      name = "Review",
      config = AppConfig.default
        .withFontConfig(FontConfig(textFontFamily = "Serif", textFontSize = 17.0f))
        .withPreferredWindowSize(PreferredWindowSize(1280, 800)),
      themeName = Theme.dark.name,
      dockedPanels = List(
        SessionDockedPanel(
          "panel-1",
          SessionPinnedPanel
            .fromPanelContent(
              PanelContent.DirectoryTree(DirectoryTreeData(root), selectedPath = Some(root)),
              PanelPosition.Right,
              44
            )
            .getOrElse(fail("directory tree panel should be capturable"))
        )
      )
    )

    val restored = UiPreset.applyToState(preset, initial, Theme.dark)

    restored.persisted.theme.name shouldBe Theme.dark.name
    restored.persisted.config.editorConfig.fontConfig.textFontFamily shouldBe "Serif"
    restored.persisted.config.editorConfig.fontConfig.textFontSize shouldBe 17.0f
    restored.persisted.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1280, 800))
    restored.pinnedSurfaces should have size 1
    restored.pinnedSurfaces.head.presentation shouldBe SurfacePresentation.Docked
    restored.persisted.layout.workspaceTree.flatMap(
      _.positionForSurface(restored.pinnedSurfaces.head.id)
    ) shouldBe Some(PanelPosition.Right)
    com.serenity.state.reducers.PanelStateReducer.currentSize(
      restored.pinnedSurfaces.head.id,
      restored
    ) shouldBe Some(44)
    restored.pinnedSurfaces.head.content shouldBe a[SurfaceContent.DirectoryTree]
  }

  it should "capture the current editor pane count" in {
    val pane0 = PaneId(0)
    val pane1 = PaneId(1)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = Map(
            pane0 -> EditorPane.withBuffer(pane0, BufferId(0)),
            pane1 -> EditorPane.empty(pane1)
          ),
          activeEditorPaneId = Some(pane0),
          workspaceTree = Some(TestWorkspaceTrees.linear(pane0, pane1))
        )
      ),
      runtime = AppState.initial.runtime.copy(nextPaneId = PaneId(2))
    )

    val preset = UiPreset.capture("Two Pane Drafting", state, preferredWindowSize = None)

    preset.targetEditorPaneCount shouldBe Some(2)
  }

  it should "provide peer prose, code, and compact workflow presets" in {
    UiPreset.builtInNames shouldBe List("Writing", "Documentation", "Code", "Compact", "Review")

    val writing = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))
    val docs    = UiPreset.builtIn("Documentation").getOrElse(fail("missing Documentation preset"))
    val code    = UiPreset.builtIn("Code").getOrElse(fail("missing Code preset"))
    val compact = UiPreset.builtIn("Compact").getOrElse(fail("missing Compact preset"))
    val review  = UiPreset.builtIn("Review").getOrElse(fail("missing Review preset"))

    writing.config.editorConfig.fontConfig.textFontFamily shouldBe Font.SERIF
    writing.config.editorConfig.fontConfig.textFontSize should be > AppConfig.default.editorConfig.fontConfig.textFontSize
    writing.config.surfaceConfig.showLineNumbers shouldBe false
    writing.config.statusLine.isPinned shouldBe false
    writing.config.surfaceConfig.motionPreset shouldBe MotionPreset.Subtle
    writing.config.surfaceConfig.editorInsertionTransitionKind shouldBe TransitionKind.TypedText
    writing.config.defaultDocumentMode shouldBe DefaultDocumentMode.RichText
    writing.targetEditorPaneCount shouldBe Some(1)
    writing.config.surfaceConfig.showPaneHeaders shouldBe false
    writing.pinnedPanels shouldBe Nil

    docs.config.markdownViewMode shouldBe MarkdownViewMode.SplitPreview
    docs.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    docs.config.surfaceConfig.editorInsertionTransitionKind shouldBe TransitionKind.LineAndCharacterTandem
    docs.targetEditorPaneCount shouldBe Some(1)
    docs.config.surfaceConfig.showPaneHeaders shouldBe false
    docs.pinnedPanels shouldBe Nil

    code.config.defaultDocumentMode shouldBe DefaultDocumentMode.PlainText
    code.config.surfaceConfig.motionPreset shouldBe MotionPreset.Reduced
    code.config.surfaceConfig.editorInsertionTransitionKind shouldBe TransitionKind.Disabled
    code.config.surfaceConfig.showLineNumbers shouldBe true
    code.config.surfaceConfig.showPaneHeaders shouldBe true
    code.pinnedPanels.map(_.position) should contain(PanelPosition.Left)

    compact.config.surfaceConfig.showLineNumbers shouldBe true
    compact.config.statusLine.isPinned shouldBe true
    compact.config.surfaceConfig.showPaneHeaders shouldBe true
    compact.config.interfaceDensity shouldBe InterfaceDensity.Compact
    compact.config.surfaceConfig.wordWrapEnabled shouldBe false
    compact.config.surfaceConfig.contextualToolbarEnabled shouldBe false
    compact.pinnedPanels shouldBe Nil

    review.pinnedPanels.map(_.content) should contain(SessionPanelContent.Diagnostics(Nil))
  }

  it should "carry the app mode each built-in workflow is for, and apply it with the workflow" in {
    val writing = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))
    val docs    = UiPreset.builtIn("Documentation").getOrElse(fail("missing Documentation preset"))
    val code    = UiPreset.builtIn("Code").getOrElse(fail("missing Code preset"))
    val compact = UiPreset.builtIn("Compact").getOrElse(fail("missing Compact preset"))
    val review  = UiPreset.builtIn("Review").getOrElse(fail("missing Review preset"))

    writing.config.appMode shouldBe AppMode.Prose
    docs.config.appMode shouldBe AppMode.Prose
    code.config.appMode shouldBe AppMode.Code
    compact.config.appMode shouldBe AppMode.Code
    review.config.appMode shouldBe AppMode.Code

    // A prose workflow picked from a code workspace must switch the workspace to prose: otherwise the settings tree
    // keeps hiding the prose groups (Document Writing, Prose Font) the workflow just made relevant.
    val fromCode = UiPreset.applyBuiltInWorkflowToState(writing, AppState.initial, Theme.dark)
    fromCode.persisted.config.appMode shouldBe AppMode.Prose

    val backToCode = UiPreset.applyBuiltInWorkflowToState(code, fromCode, Theme.dark)
    backToCode.persisted.config.appMode shouldBe AppMode.Code
  }

  it should "summarize presets for command runner previews" in {
    val writing = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))

    UiPreset.Preview.fromPreset(writing) shouldBe UiPreset.Preview(
      "Writing",
      "rich text default; dark; subtle motion; typed text reveal; frosted material; frosted background; spacious density; Serif 18pt prose; 1 editor pane"
    )
  }

  it should "restore each workflow contract when switching back to its preset" in {
    val initial = AppState.initial
    val writing = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))
    val compact = UiPreset.builtIn("Compact").getOrElse(fail("missing Compact preset"))

    val compactState = UiPreset.applyToState(compact, UiPreset.applyToState(writing, initial, Theme.dark), Theme.dark)
    val restoredWriting = UiPreset.applyToState(writing, compactState, Theme.dark)

    compactState.persisted.config shouldBe compact.config
    compactState.pinnedSurfaces shouldBe Nil
    restoredWriting.persisted.config shouldBe writing.config
    restoredWriting.pinnedSurfaces shouldBe Nil
  }

  it should "disable spell-check when applying the Code or Compact workflow, leaving prose untouched" in {
    val code    = UiPreset.builtIn("Code").getOrElse(fail("missing Code preset"))
    val compact = UiPreset.builtIn("Compact").getOrElse(fail("missing Compact preset"))
    val writing = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))
    val spellOn = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(config =
        AppState.initial.persisted.config.withSpellCheck(
          AppState.initial.persisted.config.languageToolsConfig.spellCheck.copy(enabled = true)
        )
      )
    )

    // Code workflows shouldn't spell-check (identifiers aren't prose, and there's no bundled dictionary yet #1175).
    UiPreset
      .applyBuiltInWorkflowToState(code, spellOn, Theme.dark)
      .persisted
      .config
      .languageToolsConfig
      .spellCheck
      .enabled shouldBe false
    UiPreset
      .applyBuiltInWorkflowToState(compact, spellOn, Theme.dark)
      .persisted
      .config
      .languageToolsConfig
      .spellCheck
      .enabled shouldBe false
    // Prose keeps whatever the user configured -- spell-check is appropriate there.
    UiPreset
      .applyBuiltInWorkflowToState(writing, spellOn, Theme.dark)
      .persisted
      .config
      .languageToolsConfig
      .spellCheck
      .enabled shouldBe true
  }

  it should "include editor pane count targets in command runner previews" in {
    val preset = UiPreset(
      name = "Two Pane Drafting",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      dockedPanels = Nil,
      targetEditorPaneCount = Some(2)
    )

    UiPreset.Preview.fromPreset(preset).hint shouldBe
      "plain text default; dark; smooth motion; fade text reveal; frosted material; frosted background; comfortable density; SansSerif 12pt prose; 2 editor panes"
  }

  it should "name every pinnable panel content kind in its preview summary" in {
    val panel = SessionPinnedPanel
      .fromPanelContent(PanelContent.Comments(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("comments should be capturable"))
    val preset = UiPreset(
      name = "Review",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      dockedPanels = List(SessionDockedPanel("panel-1", panel))
    )

    UiPreset.Preview.fromPreset(preset).hint should include("Left comments 28")
  }

  it should "patch appearance fields without replacing preset layout snapshots" in {
    val panel = SessionPinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config =
        AppConfig.default.withBackgroundStyle(BackgroundStyle.Solid).withInterfaceDensity(InterfaceDensity.Compact),
      themeName = Theme.dark.name,
      dockedPanels = List(SessionDockedPanel("panel-1", panel)),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default
      .withBackgroundStyle(BackgroundStyle.GlassLike)
      .withInterfaceDensity(InterfaceDensity.Spacious)
      .withUiElementGap(Some(4))
      .withUiOutlineThicknessPx(5)

    val patched = UiPreset.Patch.Appearance(sourceConfig, themeName = Some(Theme.light.name)).applyTo(preset)

    patched.config.surfaceConfig.backgroundStyle shouldBe BackgroundStyle.GlassLike
    patched.config.interfaceDensity shouldBe InterfaceDensity.Spacious
    patched.config.uiElementGap shouldBe Some(4)
    patched.config.uiOutlineThicknessPx shouldBe 5
    patched.themeName shouldBe Theme.light.name
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch motion fields without replacing preset layout snapshots" in {
    val panel = SessionPinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withMotionPreset(MotionPreset.Reduced),
      themeName = Theme.dark.name,
      dockedPanels = List(SessionDockedPanel("panel-1", panel)),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default
      .withMotionPreset(MotionPreset.Subtle)
      .withElementTransitionSpeedScale(2.25)
      .withCursorTransitionSpeedScale(Some(0.75))
      .withEditorInsertionTransitionKind(TransitionKind.TypedText)
      .withCommandRunnerTransitionKind(Some(TransitionKind.DirectionalSweep))
      .withPanelOpenTransitionKind(Some(TransitionKind.OutlineThenContent))
      .withPanelCloseTransitionKind(Some(TransitionKind.Disabled))

    val patched = UiPreset.Patch.Motion(sourceConfig).applyTo(preset)

    patched.config.surfaceConfig.motionPreset shouldBe MotionPreset.Subtle
    patched.config.editorConfig.characterAnimation shouldBe MotionPreset.Subtle.animationConfig
    patched.config.surfaceConfig.elementTransitionSpeedScale shouldBe 2.25
    patched.config.surfaceConfig.cursorTransitionSpeedScale shouldBe Some(0.75)
    patched.config.surfaceConfig.editorInsertionTransitionKind shouldBe TransitionKind.TypedText
    patched.config.surfaceConfig.commandRunnerTransitionKind shouldBe Some(TransitionKind.DirectionalSweep)
    patched.config.surfaceConfig.panelOpenTransitionKind shouldBe Some(TransitionKind.OutlineThenContent)
    patched.config.surfaceConfig.panelCloseTransitionKind shouldBe Some(TransitionKind.Disabled)
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch typography fields without replacing preset layout snapshots" in {
    val panel = SessionPinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withFontConfig(FontConfig(textFontFamily = Font.SANS_SERIF, textFontSize = 12.0f)),
      themeName = Theme.dark.name,
      dockedPanels = List(SessionDockedPanel("panel-1", panel)),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default.withFontConfig(
      FontConfig(
        codeFontFamily = Font.MONOSPACED,
        textFontFamily = Font.SERIF,
        uiFontFamily = Font.DIALOG,
        fontSize = 14.0f,
        textFontSize = 18.0f,
        uiFontSize = 13.0f,
        enableLigatures = false,
        textLigatures = true,
        uiLigatures = false
      )
    )

    val patched = UiPreset.Patch.Typography(sourceConfig).applyTo(preset)

    patched.config.editorConfig.fontConfig shouldBe sourceConfig.editorConfig.fontConfig
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch document default fields without replacing preset layout snapshots" in {
    val panel = SessionPinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default
        .withDefaultDocumentMode(DefaultDocumentMode.PlainText)
        .withMarkdownViewMode(MarkdownViewMode.Source),
      themeName = Theme.dark.name,
      dockedPanels = List(SessionDockedPanel("panel-1", panel)),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default
      .withDefaultDocumentMode(DefaultDocumentMode.Markdown)
      .withMarkdownViewMode(MarkdownViewMode.InlineLens)

    val patched = UiPreset.Patch.DocumentDefaults(sourceConfig).applyTo(preset)

    patched.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    patched.config.markdownViewMode shouldBe MarkdownViewMode.InlineLens
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch text display fields without replacing preset layout snapshots" in {
    val panel = SessionPinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default
        .withLineNumbers(true)
        .withStatusLinePlacement(StatusLinePlacement.Pinned)
        .withWordWrap(true),
      themeName = Theme.dark.name,
      dockedPanels = List(SessionDockedPanel("panel-1", panel)),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default
      .withLineNumbers(false)
      .withoutStatusLine
      .withWordWrap(false)
      .withTextAreaInsets(TextAreaInsets.fromPercent(20.0, 10.0))
      .withViewportSizing(
        ViewportSizing(
          width = ViewportAxisSizing.fromPercent(80.0, Some(120)),
          height = ViewportAxisSizing.fromPercent(90.0, Some(40))
        )
      )

    val patched = UiPreset.Patch.TextDisplay(sourceConfig).applyTo(preset)

    patched.config.surfaceConfig.showLineNumbers shouldBe false
    patched.config.statusLine.isPinned shouldBe false
    patched.config.surfaceConfig.wordWrapEnabled shouldBe false
    patched.config.surfaceConfig.textAreaInsets shouldBe TextAreaInsets.fromPercent(20.0, 10.0)
    patched.config.surfaceConfig.viewportSizing shouldBe sourceConfig.surfaceConfig.viewportSizing
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch language tool fields without replacing preset layout snapshots" in {
    val panel = SessionPinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withSpellCheck(
        SpellCheckConfig(enabled = false, languages = List("en"), additionalWords = List("serenity"))
      ),
      themeName = Theme.dark.name,
      dockedPanels = List(SessionDockedPanel("panel-1", panel)),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default.withSpellCheck(
      SpellCheckConfig(enabled = true, languages = List("EN", "fr"), additionalWords = List("Cats", "IO"))
    )

    val patched = UiPreset.Patch.LanguageTools(sourceConfig).applyTo(preset)

    patched.config.languageToolsConfig.spellCheck.enabled shouldBe true
    patched.config.languageToolsConfig.spellCheck.languages shouldBe List("en", "fr")
    patched.config.languageToolsConfig.spellCheck.additionalWords shouldBe List("cats", "io")
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }
