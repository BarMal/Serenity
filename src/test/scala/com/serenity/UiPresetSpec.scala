package com.serenity

import java.awt.Font
import java.nio.file.Files

import _root_.io.circe.parser.decode
import _root_.io.circe.syntax.*
import cats.effect.unsafe.implicits.global
import com.serenity.animation.TransitionKind
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiPresetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "UiPreset" should "capture config, theme, preferred window size, and pinned panels from app state" in {
    val root = Files.createTempDirectory("ui-preset-root")
    val config = AppConfig.default.copy(
      fontConfig = FontConfig(codeFontFamily = "Monospaced", fontSize = 18.0f),
      backgroundStyle = BackgroundStyle.GlassLike
    )
    val panel = UiSurface.fromPanelContent(
      SurfaceId("panel-1"),
      PanelContent.DirectoryTree(DirectoryTreeData(root), selectedPath = Some(root)),
      PanelPosition.Left,
      32
    )
    val state = AppState.initial.copy(
      config = config,
      theme = Theme.light,
      uiSurfaces = List(panel)
    )

    val preset = UiPreset.capture("Writing", state, Some(PreferredWindowSize(1440, 960)))

    preset.name shouldBe "Writing"
    preset.config.fontConfig.codeFontFamily shouldBe "Monospaced"
    preset.config.fontConfig.codeFontSize shouldBe 18.0f
    preset.config.backgroundStyle shouldBe BackgroundStyle.GlassLike
    preset.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1440, 960))
    preset.themeName shouldBe Theme.light.name
    preset.pinnedPanels.map(panel => panel.position -> panel.size) shouldBe List(PanelPosition.Left -> 32)
  }

  it should "restore captured config, theme, and pinned panels onto app state" in {
    val root = Files.createTempDirectory("ui-preset-restore")
    val initial = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface.fromPanelContent(
          SurfaceId("old-panel"),
          PanelContent.Diagnostics(Nil),
          PanelPosition.Bottom,
          8
        )
      )
    )
    val preset = UiPreset(
      name = "Review",
      config = AppConfig.default.copy(
        fontConfig = FontConfig(textFontFamily = "Serif", textFontSize = 17.0f),
        preferredWindowSize = Some(PreferredWindowSize(1280, 800))
      ),
      themeName = Theme.dark.name,
      pinnedPanels = List(
        UiPreset.PinnedPanel
          .fromPanelContent(
            PanelContent.DirectoryTree(DirectoryTreeData(root), selectedPath = Some(root)),
            PanelPosition.Right,
            44
          )
          .getOrElse(fail("directory tree panel should be capturable"))
      )
    )

    val restored = UiPreset.applyToState(preset, initial, Theme.dark)

    restored.theme.name shouldBe Theme.dark.name
    restored.config.fontConfig.textFontFamily shouldBe "Serif"
    restored.config.fontConfig.textFontSize shouldBe 17.0f
    restored.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1280, 800))
    restored.pinnedSurfaces should have size 1
    restored.pinnedSurfaces.head.presentation shouldBe SurfacePresentation.Pinned(PanelPosition.Right, 44)
    restored.pinnedSurfaces.head.content shouldBe a[SurfaceContent.DirectoryTree]
  }

  it should "capture the current editor pane count" in {
    val pane0 = PaneId(0)
    val pane1 = PaneId(1)
    val state = AppState.initial.copy(
      layout = Layout(
        editorPanes = Map(
          pane0 -> EditorPane.withBuffer(pane0, BufferId(0)),
          pane1 -> EditorPane.empty(pane1)
        ),
        activeEditorPaneId = Some(pane0),
        paneOrder = List(pane0, pane1)
      ),
      nextPaneId = PaneId(2)
    )

    val preset = UiPreset.capture("Two Pane Drafting", state, preferredWindowSize = None)

    preset.targetEditorPaneCount shouldBe Some(2)
  }

  it should "provide built-in task presets for writing, documentation, code, and review" in {
    UiPreset.builtInNames shouldBe List("Writing", "Documentation", "Code", "Review")

    val writing = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))
    val docs    = UiPreset.builtIn("Documentation").getOrElse(fail("missing Documentation preset"))
    val code    = UiPreset.builtIn("Code").getOrElse(fail("missing Code preset"))
    val review  = UiPreset.builtIn("Review").getOrElse(fail("missing Review preset"))

    writing.config.fontConfig.textFontFamily shouldBe Font.SERIF
    writing.config.fontConfig.textFontSize should be > AppConfig.default.fontConfig.textFontSize
    writing.config.showLineNumbers shouldBe false
    writing.config.showGutter shouldBe false
    writing.config.motionPreset shouldBe MotionPreset.Subtle
    writing.config.editorInsertionTransitionKind shouldBe TransitionKind.TypedText
    writing.config.defaultDocumentMode shouldBe DefaultDocumentMode.RichText
    writing.targetEditorPaneCount shouldBe Some(1)
    writing.pinnedPanels.map(panel => panel.position -> panel.content) should contain(
      PanelPosition.Left -> UiPreset.PanelContentSnapshot.Outline(Nil)
    )

    docs.config.markdownViewMode shouldBe MarkdownViewMode.SplitPreview
    docs.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    docs.config.editorInsertionTransitionKind shouldBe TransitionKind.LineAndCharacterTandem
    docs.targetEditorPaneCount shouldBe Some(1)
    docs.pinnedPanels.map(_.content) should contain(UiPreset.PanelContentSnapshot.Outline(Nil))

    code.config.defaultDocumentMode shouldBe DefaultDocumentMode.PlainText
    code.config.motionPreset shouldBe MotionPreset.Reduced
    code.config.editorInsertionTransitionKind shouldBe TransitionKind.Disabled
    code.config.showLineNumbers shouldBe true
    code.pinnedPanels.map(_.position) should contain(PanelPosition.Left)

    review.pinnedPanels.map(_.content) should contain(UiPreset.PanelContentSnapshot.Diagnostics(Nil))
  }

  it should "summarize presets for command runner previews" in {
    val writing = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))

    UiPreset.Preview.fromPreset(writing) shouldBe UiPreset.Preview(
      "Writing",
      "rich text default; dark; subtle motion; typed text reveal; frosted material; frosted background; spacious density; Serif 18pt prose; 1 editor pane; Left outline 28"
    )
  }

  it should "include editor pane count targets in command runner previews" in {
    val preset = UiPreset(
      name = "Two Pane Drafting",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      pinnedPanels = Nil,
      targetEditorPaneCount = Some(2)
    )

    UiPreset.Preview.fromPreset(preset).hint shouldBe
      "plain text default; dark; smooth motion; fade text reveal; frosted material; frosted background; comfortable density; SansSerif 12pt prose; 2 editor panes"
  }

  it should "patch appearance fields without replacing preset layout snapshots" in {
    val panel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config =
        AppConfig.default.withBackgroundStyle(BackgroundStyle.Solid).withInterfaceDensity(InterfaceDensity.Compact),
      themeName = Theme.dark.name,
      pinnedPanels = List(panel),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default
      .withBackgroundStyle(BackgroundStyle.GlassLike)
      .withInterfaceDensity(InterfaceDensity.Spacious)
      .withUiElementGap(4)

    val patched = UiPreset.Patch.Appearance(sourceConfig, themeName = Some(Theme.light.name)).applyTo(preset)

    patched.config.backgroundStyle shouldBe BackgroundStyle.GlassLike
    patched.config.interfaceDensity shouldBe InterfaceDensity.Spacious
    patched.config.uiElementGap shouldBe 4
    patched.themeName shouldBe Theme.light.name
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch motion fields without replacing preset layout snapshots" in {
    val panel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withMotionPreset(MotionPreset.Reduced),
      themeName = Theme.dark.name,
      pinnedPanels = List(panel),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default
      .withMotionPreset(MotionPreset.Subtle)
      .withElementTransitionSpeedScale(2.25)
      .withEditorInsertionTransitionKind(TransitionKind.TypedText)

    val patched = UiPreset.Patch.Motion(sourceConfig).applyTo(preset)

    patched.config.motionPreset shouldBe MotionPreset.Subtle
    patched.config.characterAnimation shouldBe MotionPreset.Subtle.animationConfig
    patched.config.elementTransitionSpeedScale shouldBe 2.25
    patched.config.editorInsertionTransitionKind shouldBe TransitionKind.TypedText
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch typography fields without replacing preset layout snapshots" in {
    val panel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.copy(fontConfig = FontConfig(textFontFamily = Font.SANS_SERIF, textFontSize = 12.0f)),
      themeName = Theme.dark.name,
      pinnedPanels = List(panel),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default.copy(
      fontConfig = FontConfig(
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

    patched.config.fontConfig shouldBe sourceConfig.fontConfig
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch document default fields without replacing preset layout snapshots" in {
    val panel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withDefaultDocumentMode(DefaultDocumentMode.PlainText),
      themeName = Theme.dark.name,
      pinnedPanels = List(panel),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default.withDefaultDocumentMode(DefaultDocumentMode.RichText)

    val patched = UiPreset.Patch.DocumentDefaults(sourceConfig).applyTo(preset)

    patched.config.defaultDocumentMode shouldBe DefaultDocumentMode.RichText
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch text display fields without replacing preset layout snapshots" in {
    val panel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(true)
        .withWordWrap(true),
      themeName = Theme.dark.name,
      pinnedPanels = List(panel),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default
      .withLineNumbers(false)
      .withGutter(false)
      .withWordWrap(false)
      .withTextAreaInsets(TextAreaInsets.fromPercent(20.0, 10.0))
      .withViewportSizing(
        ViewportSizing(
          width = ViewportAxisSizing.fromPercent(80.0, Some(120)),
          height = ViewportAxisSizing.fromPercent(90.0, Some(40))
        )
      )

    val patched = UiPreset.Patch.TextDisplay(sourceConfig).applyTo(preset)

    patched.config.showLineNumbers shouldBe false
    patched.config.showGutter shouldBe false
    patched.config.wordWrapEnabled shouldBe false
    patched.config.textAreaInsets shouldBe TextAreaInsets.fromPercent(20.0, 10.0)
    patched.config.viewportSizing shouldBe sourceConfig.viewportSizing
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "patch language tool fields without replacing preset layout snapshots" in {
    val panel = UiPreset.PinnedPanel
      .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Left, 28)
      .getOrElse(fail("outline should be capturable"))
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default.withSpellCheck(
        SpellCheckConfig(enabled = false, languages = List("en"), additionalWords = List("serenity"))
      ),
      themeName = Theme.dark.name,
      pinnedPanels = List(panel),
      targetEditorPaneCount = Some(1)
    )
    val sourceConfig = AppConfig.default.withSpellCheck(
      SpellCheckConfig(enabled = true, languages = List("EN", "fr"), additionalWords = List("Cats", "IO"))
    )

    val patched = UiPreset.Patch.LanguageTools(sourceConfig).applyTo(preset)

    patched.config.spellCheck.enabled shouldBe true
    patched.config.spellCheck.languages shouldBe List("en", "fr")
    patched.config.spellCheck.additionalWords shouldBe List("cats", "io")
    patched.pinnedPanels shouldBe List(panel)
    patched.targetEditorPaneCount shouldBe Some(1)
  }

  it should "collapse editor panes when a preset targets one editor pane" in {
    val primaryBufferId   = BufferId(0)
    val secondaryBufferId = BufferId(1)
    val pane0             = PaneId(0)
    val pane1             = PaneId(1)
    val secondaryBuffer   = Buffer.newEmpty(secondaryBufferId)
    val state = AppState.initial.copy(
      buffers = AppState.initial.buffers + (secondaryBufferId -> secondaryBuffer),
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
    val preset = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))

    val restored = UiPreset.applyToState(preset, state, Theme.dark)

    restored.layout.editorPanes should have size 1
    restored.layout.activeEditorPaneId shouldBe Some(pane1)
    restored.layout.paneOrder shouldBe List(pane1)
    restored.layout.editorPanes(pane1).bufferId shouldBe Some(secondaryBufferId)
    restored.buffers.keySet should contain allOf (primaryBufferId, secondaryBufferId)
    restored.bufferOrder shouldBe List(primaryBufferId, secondaryBufferId)
  }

  it should "restore editor pane count targets above one pane" in {
    val primaryBufferId   = BufferId(0)
    val secondaryBufferId = BufferId(1)
    val pane0             = PaneId(0)
    val state = AppState.initial.copy(
      buffers = AppState.initial.buffers + (secondaryBufferId -> Buffer.newEmpty(secondaryBufferId)),
      bufferOrder = List(primaryBufferId, secondaryBufferId),
      layout = Layout(
        editorPanes = Map(pane0 -> EditorPane.withBuffer(pane0, primaryBufferId)),
        activeEditorPaneId = Some(pane0),
        paneOrder = List(pane0)
      ),
      nextBufferId = BufferId(2),
      nextPaneId = PaneId(1)
    )
    val preset = UiPreset(
      name = "Two Pane Drafting",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      pinnedPanels = Nil,
      targetEditorPaneCount = Some(2)
    )

    val restored = UiPreset.applyToState(preset, state, Theme.dark)

    restored.layout.editorPanes should have size 2
    restored.layout.paneOrder shouldBe List(PaneId(0), PaneId(1))
    restored.layout.editorPanes(PaneId(0)).bufferId shouldBe Some(primaryBufferId)
    restored.layout.editorPanes(PaneId(1)).bufferId shouldBe Some(secondaryBufferId)
    restored.nextPaneId shouldBe PaneId(2)
    restored.buffers.keySet should contain allOf (primaryBufferId, secondaryBufferId)
  }

  it should "decode saved presets that do not include editor pane layout intent" in {
    import UiPreset.given

    val preset = UiPreset(
      name = "Legacy",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      pinnedPanels = Nil
    )
    val legacyJson = preset.asJson.hcursor
      .downField("targetEditorPaneCount")
      .delete
      .top
      .getOrElse(fail("expected preset json"))
      .noSpaces

    val decoded = decode[UiPreset](legacyJson).getOrElse(fail("legacy preset should decode"))

    decoded.targetEditorPaneCount shouldBe None
  }

  "UiPresetStore" should "persist named presets to disk and replace an existing preset by name" in {
    val path  = Files.createTempDirectory("ui-preset-store").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val first = UiPreset(
      name = "Focus",
      config = AppConfig.default.copy(preferredWindowSize = Some(PreferredWindowSize(1000, 700))),
      themeName = "dark",
      pinnedPanels = Nil
    )
    val second = first.copy(config = AppConfig.default.copy(preferredWindowSize = Some(PreferredWindowSize(1200, 900))))

    (for
      _       <- store.upsert(first)
      _       <- store.upsert(second)
      loaded  <- store.load()
      matched <- store.find("Focus")
    yield
      loaded.presets.map(_.name) shouldBe List("Focus")
      matched.flatMap(_.config.preferredWindowSize) shouldBe Some(PreferredWindowSize(1200, 900))
    ).unsafeRunSync()
  }

  it should "delete, rename, and duplicate custom presets" in {
    val path  = Files.createTempDirectory("ui-preset-store-management").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val focus = UiPreset(
      name = "Focus",
      config = AppConfig.default.copy(preferredWindowSize = Some(PreferredWindowSize(1000, 700))),
      themeName = "dark",
      pinnedPanels = Nil
    )
    val review = focus.copy(name = "Review")

    (for
      _       <- store.upsert(focus)
      _       <- store.upsert(review)
      _       <- store.duplicate("Focus", "Focus Copy")
      _       <- store.rename("Review", "Review Notes")
      _       <- store.delete("Focus")
      loaded  <- store.load()
      copied  <- store.find("Focus Copy")
      renamed <- store.find("Review Notes")
    yield
      loaded.names.sorted shouldBe List("Focus Copy", "Review Notes")
      copied.flatMap(_.config.preferredWindowSize) shouldBe Some(PreferredWindowSize(1000, 700))
      renamed.map(_.themeName) shouldBe Some("dark")
    ).unsafeRunSync()
  }
