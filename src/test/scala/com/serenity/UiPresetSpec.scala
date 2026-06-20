package com.serenity

import java.awt.Font
import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{DirectoryTreeData, PanelContent, PanelPosition}
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
    writing.pinnedPanels.map(panel => panel.position -> panel.content) should contain(
      PanelPosition.Left -> UiPreset.PanelContentSnapshot.Outline(Nil)
    )

    docs.config.markdownViewMode shouldBe MarkdownViewMode.SplitPreview
    docs.pinnedPanels.map(_.content) should contain(UiPreset.PanelContentSnapshot.Outline(Nil))

    code.config.motionPreset shouldBe MotionPreset.Reduced
    code.config.showLineNumbers shouldBe true
    code.pinnedPanels.map(_.position) should contain(PanelPosition.Left)

    review.pinnedPanels.map(_.content) should contain(UiPreset.PanelContentSnapshot.Diagnostics(Nil))
  }

  it should "summarize presets for command runner previews" in {
    val writing = UiPreset.builtIn("Writing").getOrElse(fail("missing Writing preset"))

    UiPreset.Preview.fromPreset(writing) shouldBe UiPreset.Preview(
      "Writing",
      "dark; subtle motion; frosted material; Serif 18pt prose; Left outline 28"
    )
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
