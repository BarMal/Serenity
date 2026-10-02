package com.serenity.keystroke.translators

import com.serenity.config.{AppConfig, ModalKeyAction}
import com.serenity.keystroke.events.ModalInputEvent

/** Only the modal keymap's move-item keys, for a reorderable list to claim ahead of the global hotkeys. */
class ItemMoveTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[ModalInputEvent]:

  private val moveActions: Set[ModalKeyAction] = Set(ModalKeyAction.MoveItemUp, ModalKeyAction.MoveItemDown)

  override lazy val converters = List(
    LocalKeymapConverters.converter(
      appConfig.inputConfig.focusedKeymapConfig.modal.bindings.view.filterKeys(moveActions).toMap
    )
  )
