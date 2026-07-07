package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandIntent, CommandRegistry}
import com.serenity.keystroke.events.{Enter, InsertChar}
import com.serenity.rope.Balance
import com.serenity.state.components.ThemeCreatorComponent
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEffect
import com.serenity.ui.layout.{Layout, LayoutRect}
import com.serenity.ui.renderer.{OverlayRowLayout, SurfaceContentResolver, SurfaceRenderMode}
import com.serenity.ui.theme.DefaultThemes
import com.serenity.ui.theme.config.{ThemeConfigLoader, ThemeConfigWriter, ThemeCreatorState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThemeCreatorSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ThemeConfigWriter" should "write user theme configs that the loader can read" in {
    val config = ThemeConfigWriter.themeToConfig(DefaultThemes.defaultDark.copy(name = "quiet-focus"))
    val file   = Files.createTempFile("serenity-theme", ".conf")

    try
      ThemeConfigWriter.write(config, file).unsafeRunSync()
      val loaded = ThemeConfigLoader().loadThemeFromFile(file).unsafeRunSync()

      loaded.name shouldBe "quiet-focus"
      loaded.ui.background shouldBe config.ui.background
      loaded.ui.margin shouldBe config.ui.margin
      loaded.ui.panelBorder shouldBe config.ui.panelBorder
      loaded.syntax.keyword.foreground shouldBe config.syntax.keyword.foreground
    finally Files.deleteIfExists(file)
  }

  "ThemeCreatorState" should "derive editable colour rows from the current theme" in {
    val state = ThemeCreatorState.fromTheme(DefaultThemes.defaultDark)

    state.rows.map(_.path) should contain allOf (
      "theme.name",
      "ui.background",
      "ui.margin",
      "ui.panel-border",
      "ui.panel.background",
      "syntax.keyword.foreground"
    )
    state.selectedRow.map(_.path) shouldBe Some("theme.name")
  }

  it should "preview valid colour edits as a Theme" in {
    val state = ThemeCreatorState
      .fromTheme(DefaultThemes.defaultDark)
      .selectPath("ui.background")
      .replaceSelectedValue("#123456")

    state.previewTheme.map(_.background.getRGB & 0x00ffffff) shouldBe Right(0x123456)
  }

  it should "keep invalid colour edits in the draft without producing a preview theme" in {
    val state = ThemeCreatorState
      .fromTheme(DefaultThemes.defaultDark)
      .selectPath("ui.background")
      .replaceSelectedValue("not-a-colour")

    state.previewTheme.left.map(_.contains("Unknown color")) shouldBe Left(true)
  }

  "SurfaceContentResolver" should "render theme creator rows with editable values and colour preview segments" in {
    val creator = ThemeCreatorState
      .fromTheme(DefaultThemes.defaultDark)
      .selectPath("ui.background")

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.ThemeCreator(creator),
      LayoutRect(0, 0, 80, 12),
      SurfaceRenderMode.Floating
    )

    resolved.header.map(_.plainText) shouldBe Some("theme creator")
    resolved.rows.exists(row => row.plainText.contains("Background")) shouldBe true
    val backgroundRow = resolved.rows.find(_.plainText.contains("Background")).get
    backgroundRow.selected shouldBe true
    backgroundRow.layout shouldBe OverlayRowLayout.Columns
    backgroundRow.segments.last.backgroundColor shouldBe defined
  }

  it should "keep the selected theme creator row inside a short visible window" in {
    val creator = ThemeCreatorState
      .fromTheme(DefaultThemes.defaultDark)
      .selectPath("syntax.normal.foreground")

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.ThemeCreator(creator),
      LayoutRect(0, 0, 80, 8),
      SurfaceRenderMode.Floating
    )

    resolved.rows should have size 5
    resolved.rows.exists(_.plainText.contains("Normal Text")) shouldBe true
    resolved.rows.exists(_.selected) shouldBe true
    resolved.rows.head.plainText should not include "Theme Name"
  }

  "ThemeCreatorComponent" should "edit the selected field and apply a live preview theme" in {
    val base = AppState.empty.copy(
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), BufferId(0))),
        activeEditorPaneId = Some(PaneId(0))
      ),
      buffers = Map(BufferId(0) -> Buffer.newEmpty(BufferId(0))),
      theme = DefaultThemes.defaultDark
    )
    val creator = ThemeCreatorState
      .fromTheme(DefaultThemes.defaultDark)
      .selectPath("ui.background")
      .replaceSelectedValue("")
    val surface = UiSurface(
      SurfaceId("theme-creator"),
      SurfaceContent.ThemeCreator(creator),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = base.copy(uiSurfaces = List(surface), focus = Focus.Surface(surface.id))

    val result = List('#', '1', '2', '3', '4', '5', '6').foldLeft(state) { (current, char) =>
      val updated = ThemeCreatorComponent().processEvent(InsertChar(char), current)
      updated match
        case com.serenity.state.components.ComponentResult.StateChange(f) => f(current)
        case other                                                        => fail(s"Expected StateChange, got $other")
    }

    result.theme.background.getRGB & 0x00ffffff shouldBe 0x123456
    result.themeCreatorSurface
      .flatMap(_.content match
        case SurfaceContent.ThemeCreator(draft) => draft.selectedRow.map(_.value)
        case _                                  => None) shouldBe Some("#123456")
  }

  it should "submit a save effect for a valid draft" in {
    val base = AppState.empty.copy(theme = DefaultThemes.defaultDark)
    val creator = ThemeCreatorState
      .fromTheme(DefaultThemes.defaultDark.copy(name = "quiet-focus"))
      .selectPath("theme.name")
    val surface = UiSurface(
      SurfaceId("theme-creator"),
      SurfaceContent.ThemeCreator(creator),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = base.copy(uiSurfaces = List(surface), focus = Focus.Surface(surface.id))

    val result = ThemeCreatorComponent().processEvent(Enter, state)

    val reducerResult = result match
      case com.serenity.state.components.ComponentResult.ReducerUpdate(value) => value
      case other => fail(s"Expected ReducerUpdate, got $other")
    reducerResult.effects.collect { case AppEffect.SaveThemeConfig(config) => config.name } shouldBe List("quiet-focus")
  }

  "CommandRegistry" should "expose a theme creator command" in {
    CommandRegistry.default.findCommand("theme-creator").map(_.intent) shouldBe Some(CommandIntent.OpenThemeCreator)
  }

  it should "expose a theme export command" in {
    CommandRegistry.default.findCommand("export-theme").map(_.intent) shouldBe Some(CommandIntent.ExportCurrentTheme)
  }

end ThemeCreatorSpec
