package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{ColumnLeft, ColumnRight, EditorEvent, PageDown, PageUp}

class EditorInputTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[EditorEvent]:

  // Column-based document layout (issue #1338, Phase 1): PageUp/PageDown become their column-mode counterparts only
  // while column mode and word wrap are both on -- otherwise a no-op, leaving both keys' ordinary vertical-page
  // events untouched (including under a custom keymap override, since this remaps whatever event the configured
  // binding resolved to, not just the default key).
  private val columnModeActive =
    appConfig.surfaceConfig.columnModeEnabled && appConfig.surfaceConfig.wordWrapEnabled

  private def remapForColumnMode(event: EditorEvent): EditorEvent =
    if !columnModeActive then event
    else
      event match
        case PageUp   => ColumnLeft
        case PageDown => ColumnRight
        case other    => other

  override lazy val converters = List(
    LocalKeymapConverters
      .converter(appConfig.inputConfig.focusedKeymapConfig.editor.bindings)
      .andThen(remapForColumnMode),
    TextCharacterConverters.characterConverter
  )
