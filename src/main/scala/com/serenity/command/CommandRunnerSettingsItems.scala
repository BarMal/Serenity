package com.serenity.command

import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.serenity.ui.presets.UiPreset

/** Builds command-runner settings option rows and static settings command rows.
  *
  * Cursor, motion, appearance, panel, and text-display settings items live in sibling
  * `CommandRunnerSettings*Items` objects in this package -- split out to keep every file under the
  * architecture size targets. `boundedOptionIndex`/`enabledOptionItem` stay here, `private[command]`, since
  * this is the object most of those siblings already depend on.
  */
object CommandRunnerSettingsItems:

  private[command] def uiPresetSelectOptionItem(
    previews: List[UiPreset.Preview],
    optionSelections: Map[String, Int] = Map.empty
  ): CommandSurfaceItem.OptionItem =
    val builtInOptions = UiPreset.builtIns.map { preset =>
      val preview = UiPreset.Preview.fromPreset(preset)
      CommandOption(
        preview.name,
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(preview.name)),
        hint = Some(preview.hint)
      )
    }
    val customOptions = normalizedUiPresetPreviews(previews).map { preview =>
      CommandOption(
        preview.name,
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(preview.name)),
        hint = Some(preview.hint)
      )
    }
    val options = builtInOptions ++ customOptions
    val selectedIndex =
      optionSelections
        .get("ui-preset-custom")
        .filter(_ => customOptions.nonEmpty)
        .map(index => builtInOptions.size + boundedOptionIndex(index, customOptions))
        .getOrElse(boundedOptionIndex(optionSelections.getOrElse("ui-preset-built-in", 0), builtInOptions))

    CommandSurfaceItem.OptionItem(
      id = "ui-preset-select",
      label = "Select Preset",
      options = options,
      selectedIndex = boundedOptionIndex(selectedIndex, options),
      category = CommandCategory.Settings,
      hint = Some("Built-in and saved presets")
    )

  private[command] def markdownViewOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "markdown-view",
      label = "Markdown View",
      options = List(
        CommandOption("Source", CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.Source))),
        CommandOption(
          "Split Preview",
          CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.SplitPreview))
        ),
        CommandOption("Inline Lens", CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.InlineLens)))
      ),
      selectedIndex = optionSelections.getOrElse("markdown-view", 0),
      category = CommandCategory.Settings,
      hint = Some("Source, side preview, or inline editing lens")
    )

  private[command] def defaultDocumentModeOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "default-document-mode",
      label = "Default Document",
      options = List(
        CommandOption(
          "Plain Text",
          CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.PlainText))
        ),
        CommandOption("Markdown", CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown))),
        CommandOption("Rich Text", CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText)))
      ),
      selectedIndex = optionSelections.getOrElse("default-document-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Mode for newly-created documents")
    )

  private[command] def appModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "app-mode",
      label = "App Mode",
      options = List(
        CommandOption("Code", CommandIntent.View(ViewIntent.SetAppMode(AppMode.Code))),
        CommandOption("Prose", CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose)))
      ),
      selectedIndex = optionSelections.getOrElse("app-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Code or prose workspace -- filters which settings are shown below")
    )

  private[command] def showAllSettingsOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "settings-show-all",
      label = "Show All Settings",
      options = List(
        CommandOption("Off", CommandIntent.View(ViewIntent.SetShowAllSettingsRegardlessOfMode(false))),
        CommandOption("On", CommandIntent.View(ViewIntent.SetShowAllSettingsRegardlessOfMode(true)))
      ),
      selectedIndex = optionSelections.getOrElse("settings-show-all", 0),
      category = CommandCategory.Settings,
      hint = Some("Show settings hidden by the app mode filter above")
    )

  private[command] def spellCheckOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "spellcheck-enabled",
      label = "Spell Check",
      options = List(
        CommandOption(
          "Off",
          CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckEnabled(false)))
        ),
        CommandOption(
          "On",
          CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckEnabled(true)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("spellcheck-enabled", 0),
      category = CommandCategory.Settings,
      hint = Some("Check prose buffers")
    )

  private[command] def boundedOptionIndex(index: Int, options: List[CommandOption]): Int =
    if options.isEmpty then 0
    else index.max(0).min(options.length - 1)

  private[command] def enabledOptionItem(
    id: String,
    label: String,
    selectedIndex: Int,
    enabledIntent: CommandIntent,
    disabledIntent: CommandIntent,
    hint: String
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = id,
      label = label,
      options = List(
        CommandOption("On", enabledIntent),
        CommandOption("Off", disabledIntent)
      ),
      selectedIndex = selectedIndex,
      category = CommandCategory.Settings,
      hint = Some(hint)
    )

  private[command] def normalizedUiPresetNames(names: List[String]): List[String] =
    names
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinctBy(_.toLowerCase)
      .sortBy(_.toLowerCase)

  private[command] def normalizedUiPresetPreviews(previews: List[UiPreset.Preview]): List[UiPreset.Preview] =
    previews
      .map(preview => preview.copy(name = preview.name.trim, hint = preview.hint.trim))
      .filter(_.name.nonEmpty)
      .distinctBy(_.name.toLowerCase)
      .sortBy(_.name.toLowerCase)

  private[command] def codeFontGroupItem(
    optionSelections: Map[String, Int],
    availableFamilies: List[String] = FontLoader.availableMonospaceFamilies
  ): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "code-font",
      label = "Code Font",
      selectedIndex = optionSelections.getOrElse("code-font", 0),
      families = availableFamilies,
      intent =
        commandIntentArg => CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeFontFamily(commandIntentArg))),
      hint = "Used in code buffers"
    )

  private[command] def textFontGroupItem(
    optionSelections: Map[String, Int],
    availableFamilies: List[String] = FontLoader.availableTextFamilies
  ): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "text-font",
      label = "Text Font",
      selectedIndex = optionSelections.getOrElse("text-font", 0),
      families = availableFamilies,
      intent =
        commandIntentArg => CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextFontFamily(commandIntentArg))),
      hint = "Used in prose buffers"
    )

  private[command] def uiFontGroupItem(
    optionSelections: Map[String, Int],
    availableFamilies: List[String] = FontLoader.availableUiFamilies
  ): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "ui-font",
      label = "UI Font",
      selectedIndex = optionSelections.getOrElse("ui-font", 0),
      families = availableFamilies,
      intent =
        commandIntentArg => CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiFontFamily(commandIntentArg))),
      hint = "Used in the app interface"
    )

  private[command] def textScaleModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    val options = List(
      CommandOption(
        "Auto",
        CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextScaleMode(TextScaleMode.Auto))),
        Some("Use display transform")
      ),
      CommandOption(
        "Manual",
        CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextScaleMode(TextScaleMode.Manual))),
        Some("Use configured multiplier")
      ),
      CommandOption(
        "Off",
        CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextScaleMode(TextScaleMode.Off))),
        Some("Use unscaled point sizes")
      )
    )
    CommandSurfaceItem.OptionItem(
      id = "text-scale-mode",
      label = "Text Scale Mode",
      options = options,
      selectedIndex = boundedOptionIndex(optionSelections.getOrElse("text-scale-mode", 0), options),
      category = CommandCategory.Settings,
      hint = Some("How font sizes adapt to the display")
    )

  private def fontFamilyGroupItem(
    id: String,
    label: String,
    selectedIndex: Int,
    families: List[String],
    intent: String => CommandIntent,
    hint: String
  ): CommandSurfaceItem.GroupItem =
    val selectedFamily = families.lift(selectedIndex).orElse(families.headOption).getOrElse("")
    val children = families.zipWithIndex.map {
      case (family, index) =>
        CommandSurfaceItem.CommandItem(
          Command.typed(
            s"$id-$index-${family.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")}",
            hint,
            intent(family),
            CommandCategory.Settings,
            label = family
          )
        )
    }
    CommandSurfaceItem.GroupItem(
      id = id,
      label = label,
      children = children,
      category = CommandCategory.Settings,
      hint = Some(selectedFamily)
    )

  private[command] def codeLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "code-ligatures",
      label = "Code Ligatures",
      options = List(
        CommandOption("On", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeLigatures(true)))),
        CommandOption("Off", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeLigatures(false))))
      ),
      selectedIndex = optionSelections.getOrElse("code-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] def textLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "text-ligatures",
      label = "Prose Ligatures",
      options = List(
        CommandOption("On", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextLigatures(true)))),
        CommandOption("Off", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextLigatures(false))))
      ),
      selectedIndex = optionSelections.getOrElse("text-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] def uiLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "ui-ligatures",
      label = "UI Ligatures",
      options = List(
        CommandOption("On", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiLigatures(true)))),
        CommandOption("Off", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiLigatures(false))))
      ),
      selectedIndex = optionSelections.getOrElse("ui-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  // issue #1057: this built the "Current Buffer Language" settings group's rows. Removed -- buffer-language
  // switchers are one-shot actions with no persisted "current" value shown in their own row, and are now registered
  // directly as `CommandRegistry.languageCommands`, reachable only via the palette.
