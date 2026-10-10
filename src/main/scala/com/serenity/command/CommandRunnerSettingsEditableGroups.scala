package com.serenity.command

import com.serenity.ui.fonts.FontLoader

/** The settings groups that exist twice: once for the live settings, and once more per stored preset, which can hold
  * exactly these. Built from whichever `optionSelections` and `inputItems` the caller wants shown.
  */
final private[command] case class CommandRunnerSettingsEditableGroups(
    cursor: CommandSurfaceItem.GroupItem,
    proseFont: CommandSurfaceItem.GroupItem,
    codeFont: CommandSurfaceItem.GroupItem,
    uiFont: CommandSurfaceItem.GroupItem,
    documentDefaults: CommandSurfaceItem.GroupItem,
    spellCheck: CommandSurfaceItem.GroupItem
)

private[command] object CommandRunnerSettingsEditableGroups:

  def build(
    optionSelections: Map[String, Int],
    inputItems: List[CommandSurfaceItem.InputItem],
    fontFamilies: FontLoader.FontFamilyCatalog
  ): CommandRunnerSettingsEditableGroups =
    def input(ids: String*): List[CommandSurfaceItem.InputItem] =
      ids.toList.flatMap(id => inputItems.find(_.id == id))
    def group(id: String, label: String, hint: String, children: List[CommandSurfaceItem]) =
      CommandSurfaceItem.GroupItem(id, label, children, CommandCategory.Settings, Some(hint))

    val fontHint = "Family, size, ligatures"
    CommandRunnerSettingsEditableGroups(
      cursor = group(
        "settings-cursor",
        "Cursor",
        "Caret style",
        List(CommandRunnerSettingsCursorItems.cursorModeOptionItem(optionSelections))
      ),
      proseFont = group(
        "settings-prose-font",
        "Prose Font",
        fontHint,
        List(
          CommandRunnerSettingsItems.textFontGroupItem(optionSelections, fontFamilies.text),
          CommandRunnerSettingsItems.textLigaturesOptionItem(optionSelections)
        ) ++ input("text-font-size")
      ),
      codeFont = group(
        "settings-code-font",
        "Code Font",
        fontHint,
        List(
          CommandRunnerSettingsItems.codeFontGroupItem(optionSelections, fontFamilies.monospace),
          CommandRunnerSettingsItems.codeLigaturesOptionItem(optionSelections)
        ) ++ input("code-font-size")
      ),
      uiFont = group(
        "settings-ui-font",
        "UI Font",
        fontHint,
        List(
          CommandRunnerSettingsItems.uiFontGroupItem(optionSelections, fontFamilies.ui),
          CommandRunnerSettingsItems.uiLigaturesOptionItem(optionSelections)
        ) ++ input("ui-font-size")
      ),
      documentDefaults = group(
        "settings-document-defaults",
        "Document Defaults",
        "New document mode, Markdown view, and drop caps",
        List(
          CommandRunnerSettingsItems.defaultDocumentModeOptionItem(optionSelections),
          CommandRunnerSettingsItems.markdownViewOptionItem(optionSelections),
          CommandRunnerSettingsItems.dropCapsEnabledOptionItem(optionSelections)
        )
      ),
      spellCheck = group(
        "settings-spellcheck",
        "Spell Check",
        "Enable, languages, dictionaries, accepted words",
        CommandRunnerSettingsItems.spellCheckOptionItem(optionSelections) ::
          input("spellcheck-languages", "spellcheck-dictionaries", "spellcheck-words")
      )
    )
