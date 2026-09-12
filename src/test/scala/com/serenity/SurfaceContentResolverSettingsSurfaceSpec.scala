package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverSettingsSurfaceSpec extends AnyFlatSpec with Matchers:

  "SurfaceContentResolver" should "render direct settings search result rows with effective values and source scopes" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .updateSearchTerm("default document")

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 90, 10),
      SurfaceRenderMode.Floating
    )

    val row = floating.rows
      .find(_.plainText.contains("Default Document"))
      .getOrElse(fail("Expected direct document mode result"))

    row.segments.headOption.map(_.text) shouldBe Some("Default Document")
    row.segments.map(_.text) shouldBe List(
      "Default Document",
      "Plain Text",
      "Global",
      "Settings > Document Writing > Document Defaults"
    )
  }

  it should "resolve option rows into label, hint, and selected value columns" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        optionSelections = Map("interface-density" -> 1),
        surface = CommandRunnerSurface
          .Settings(drilled = Some(SettingsSurfaceState(SettingsPage.Group("settings-interface-layout"))))
      )

    val row = SurfaceContentResolver
      .resolve(
        SurfaceContent.CommandPalette(runner),
        LayoutRect(0, 0, 80, 10),
        SurfaceRenderMode.Floating
      )
      .rows
      .headOption
      .getOrElse(fail("Expected interface density option row"))

    row.layout shouldBe OverlayRowLayout.Columns
    row.plainText shouldBe "Interface Density: Compact, comfortable, or spacious Comfortable"
    row.segments.map(_.text) shouldBe List("Interface Density", "Compact, comfortable, or spacious", "Comfortable")
    row.segments.filter(_.selected).map(_.text) shouldBe List("Comfortable")
  }

  it should "render nested submenu headers as breadcrumbs" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface.Settings(drilled =
          Some(
            SettingsSurfaceState(
              SettingsPage.Group("settings-preset-prose-font"),
              List(SettingsPage.Group("settings-preset-edit"), SettingsPage.Group("settings-ui-presets"))
            )
          )
        )
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 10),
      SurfaceRenderMode.Floating
    )

    val header = resolved.header.getOrElse(fail("Expected breadcrumb header"))
    header.plainText shouldBe "Settings > UI Presets > Edit Preset: Writing > Prose Font"
    header.segments.map(_.text) shouldBe List("Settings >", "UI Presets >", "Edit Preset: Writing >", "Prose Font")
    header.segments.map(_.selected) shouldBe List(true, true, true, false)
  }

  /** Bug: selecting a settings-root group whose expand-in-place preview (`groupPreviewRows`, capped at
    * `CommandRunner.MaxPreviewRows` = 4) needs the whole remaining item budget left `SurfaceFrameLayout.itemWindow`
    * computing zero visible item rows -- `visibleItemRows` subtracts `reservedContentRows` from the same row budget
    * shared with sibling items, and at the default Comfortable density (`itemTargetRows = 2` for `CommandPalette`,
    * matching production's `itemTargetRowsFor`) plus the default-on persistent key-hint row
    * (`commandRunnerShowKeyHints`, matching `showKeyHintsFor`), four or more preview rows exhausts the budget entirely,
    * before the *selected* item's own row is accounted for. The whole settings list then rendered as no rows at all --
    * not even the selected group -- rather than just dropping unselected sibling rows to make room. "Document Writing"
    * (4 children: Navigation, Document Defaults, Rich Text, Spell Check) is the first settings-root group hit at this
    * window size, with these (production-matching) rendering parameters.
    */
  it should "always render the selected settings-root group's own row, even when its preview needs the whole item budget" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .copy(surface = CommandRunnerSurface.Settings(root = CommandPaletteState(selectedIndex = 2), drilled = None))

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 10),
      SurfaceRenderMode.Floating,
      itemGapRows = 0.0,
      itemTargetRows = 2,
      showKeyHints = true
    )

    resolved.rows should not be empty
    resolved.rows.find(_.selected).map(_.plainText) shouldBe Some("Document Writing")
  }

  // issue #1058: "Fonts" (Editor/Code/UI Typography) is gone -- editing a preset's typography now drills straight
  // into the same canonical Prose/Code/UI Font groups the top-level Typography settings use, each holding its own
  // family picker, ligatures toggle, and size input directly, rather than one extra wrapping level per font family.
  it should "resolve preset prose font submenu as the canonical font family, ligature, and size rows" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface.Settings(drilled =
          Some(
            SettingsSurfaceState(
              SettingsPage.Group("settings-preset-prose-font"),
              List(SettingsPage.Group("settings-preset-edit"), SettingsPage.Group("settings-ui-presets"))
            )
          )
        )
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 20),
      SurfaceRenderMode.Floating
    )

    resolved.rows.flatMap(_.segments.headOption.map(_.text)) should contain allOf (
      "Text Font",
      "Prose Ligatures",
      "Prose Font Size"
    )
  }

  // issue #1057: Theme Chooser/Creator/Toggle/Reload are one-shot actions with no preset-scoped value of their own,
  // so they are ordinary CommandRegistry commands, not part of this settings subtree.
  // issue #1058: "Theme & Surface > Surface Material" is gone -- editing a preset's surface appearance now drills
  // straight into the same canonical Surface Appearance group the top-level Settings screen uses.
  it should "resolve preset surface appearance submenu as the canonical surface appearance rows" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface.Settings(drilled =
          Some(
            SettingsSurfaceState(
              SettingsPage.Group("settings-preset-surface-appearance"),
              List(SettingsPage.Group("settings-preset-edit"), SettingsPage.Group("settings-ui-presets"))
            )
          )
        )
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 10),
      SurfaceRenderMode.Floating
    )

    resolved.rows.flatMap(_.segments.headOption.map(_.text)) shouldBe List(
      "Background Style",
      "Material Preset",
      "Post-processing",
      "Menu & Panel Shadows",
      "Blur Radius"
    )
  }

  // issue #1058: "Document Defaults > New Documents/Markdown Preview/Spelling" is gone -- editing a preset's document
  // defaults now drills straight into the same canonical Document Defaults group (default mode + Markdown view);
  // Spelling is now its own sibling page (`settings-preset-spellcheck`), matching the top-level Document Writing tree.
  it should "resolve preset document defaults submenu as the canonical document default rows" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface.Settings(drilled =
          Some(
            SettingsSurfaceState(
              SettingsPage.Group("settings-preset-document-defaults"),
              List(SettingsPage.Group("settings-preset-edit"), SettingsPage.Group("settings-ui-presets"))
            )
          )
        )
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 80, 10),
      SurfaceRenderMode.Floating
    )

    resolved.rows.flatMap(_.segments.headOption.map(_.text)) shouldBe List("Default Document", "Markdown View")
  }

  it should "expand the selected preset group's own children inline as the capped group preview" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(
        surface = CommandRunnerSurface
          .Settings(drilled = Some(SettingsSurfaceState(SettingsPage.Group("settings-ui-presets", 1))))
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 90, 12),
      SurfaceRenderMode.Floating
    )

    val selectedIndex = resolved.rows.indexWhere(_.selected)
    selectedIndex should be >= 0
    val previewRow = resolved.rows.lift(selectedIndex + 1).getOrElse(fail("Expected a group preview row"))
    previewRow.leadingPadding shouldBe 2
    previewRow.segments.map(_.tone) should contain(OverlayTone.Muted)
  }

  it should "render the settings-group-browsing key hint, distinct from the transient action-word footer" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(surface = CommandRunnerSurface.Settings(drilled = None))

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 90, 12),
      SurfaceRenderMode.Floating,
      showKeyHints = true
    )

    resolved.keyHintRow.map(_.plainText) shouldBe Some("↑↓ navigate • Enter open • Esc back • ←→ cycle option")
    resolved.footer shouldBe None
  }

  it should "render the editing-value key hint while a settings input is mid-edit" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(surface =
        CommandRunnerSurface.Settings(drilled =
          Some(
            SettingsSurfaceState(
              SettingsPage.Editing(groupId = "settings-interface-layout", itemId = "ui-element-gap", draftText = "3")
            )
          )
        )
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 90, 12),
      SurfaceRenderMode.Floating,
      showKeyHints = true
    )

    resolved.keyHintRow.map(_.plainText) shouldBe Some("Type to edit • Enter save • Esc cancel")
  }

  it should "render the recording-a-keybinding key hint while a keybinding is mid-record" in {
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(surface =
        CommandRunnerSurface.Settings(drilled =
          Some(
            SettingsSurfaceState(
              SettingsPage.Editing(
                groupId = "settings-keymap",
                itemId = "keymap-command-runner-submit",
                draftText = "",
                recording = Some(RecordingState(itemId = "keymap-command-runner-submit"))
              )
            )
          )
        )
      )

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 90, 12),
      SurfaceRenderMode.Floating,
      showKeyHints = true
    )

    resolved.keyHintRow.map(_.plainText) shouldBe Some("Esc cancel")
  }

  it should "reserve group preview rows without clipping available submenu items or footer" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .copy(surface =
        CommandRunnerSurface
          .Settings(drilled = Some(SettingsSurfaceState(SettingsPage.Group("settings-ui-presets", 1))))
      )
    val items        = runner.submenuItems("settings-ui-presets")
    val preview      = SettingsSurfaceState.previewRows(items, 1)
    val previewCount = preview.rows.size + (if preview.overflowCount > 0 then 1 else 0)
    val rect = LayoutRect(
      x = 0,
      y = 0,
      width = 90,
      height = SurfaceFrameLayout.frameHeightForItemRows(
        itemRows = items.size,
        hasHeader = true,
        hasFooter = true,
        reservedContentRows = previewCount
      )
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      rect,
      SurfaceRenderMode.Floating
    )

    floating.header shouldBe defined
    floating.rows.count(row => row.leadingPadding == 0) shouldBe items.size
    floating.rows.count(row => row.leadingPadding > 0) shouldBe previewCount
    floating.footer shouldBe defined
  }

  it should "mark font submenu labels with their preview font family" in {
    val family = FontLoader.availableTextFamilies.head
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(surface =
        CommandRunnerSurface.Settings(drilled = Some(SettingsSurfaceState(SettingsPage.Group("text-font"))))
      )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 60, 8),
      SurfaceRenderMode.Floating
    )

    val firstRow = floating.rows.head
    firstRow.plainText should include(family)
    firstRow.segments.headOption.flatMap(_.fontFamily) shouldBe Some(family)
  }

end SurfaceContentResolverSettingsSurfaceSpec
