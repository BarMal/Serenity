package com.serenity.command

import com.serenity.keystroke.KeyStrokeInfo

/** `CommandRunner` methods for editing a settings submenu item in place -- moving the submenu selection,
  * beginning/ending a text edit or keybinding recording, and adjusting an option in place. Split out of `CommandRunner`
  * to keep both under the architecture size targets -- see that class's doc.
  */
private[command] trait CommandRunnerSubmenuEditing:
  self: CommandRunner =>

  def moveSubmenuSelection(delta: Int): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        val items   = filteredPageItems(surface.current, submenuItems(groupId))
        if items.isEmpty then this
        else
          val itemCount    = items.size
          val newIndex     = (pageSelectedIndex(surface.current) + delta) % itemCount
          val wrappedIndex = if newIndex < 0 then itemCount + newIndex else newIndex
          copy(
            submenuSelections = submenuSelections + (groupId -> wrappedIndex),
            // Moving selection always exits edit mode, so the new current page is always rebuilt as a Group,
            // dropping any Editing page that was there.
            surface = CommandRunnerSurface.Settings(
              root = rootState,
              drilled =
                Some(surface.copy(current = SettingsPage.Group(groupId, wrappedIndex, surface.current.searchTerm)))
            )
          )
      case None => this

  def beginSubmenuEditMode: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        val items   = filteredPageItems(surface.current, submenuItems(groupId))
        items.lift(pageSelectedIndex(surface.current)) match
          case Some(item: CommandSurfaceItem.InputItem) =>
            withDrilledSettingsSurface(
              surface.copy(current =
                SettingsPage.Editing(
                  groupId = groupId,
                  itemId = item.id,
                  draftText = item.currentValue,
                  searchTerm = surface.current.searchTerm
                )
              )
            )
          case _ =>
            this
      case None =>
        this

  /** Sets the currently-edited item's draft text, beginning a fresh edit of `itemId` if nothing (or a different item)
    * was being edited. Used for both starting an edit from a single keystroke and continuing one (`RunnerInsertChar`),
    * so it always writes `itemId` rather than assuming the previous one still applies.
    */
  def withSubmenuEditingItem(itemId: String, text: String): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        withDrilledSettingsSurface(
          surface.copy(current =
            SettingsPage.Editing(
              groupId = surface.current.groupId,
              itemId = itemId,
              draftText = text,
              searchTerm = surface.current.searchTerm
            )
          )
        )
      case None =>
        this

  /** Replaces the currently-edited item's draft text in place (word-delete, not character Backspace -- see
    * `deleteSubmenuTextBackward` for that). A no-op when nothing is being edited.
    */
  def withSubmenuEditingText(text: String): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        surface.current match
          case editing: SettingsPage.Editing =>
            withDrilledSettingsSurface(surface.copy(current = editing.copy(draftText = text)))
          case _ =>
            this
      case None =>
        this

  /** Cancels an in-progress edit without touching any pending recording/conflict state or navigating -- Escape's
    * "cancel this edit, stay on this page" behavior. Contrast `clearSubmenuEditingAndRecording`, which also clears
    * recording state (used once a value has actually been submitted or a recording finished).
    */
  def cancelSubmenuEditingText: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        withDrilledSettingsSurface(
          surface.copy(current =
            SettingsPage.Group(surface.current.groupId, pageSelectedIndex(surface.current), surface.current.searchTerm)
          )
        )
      case None =>
        this

  /** Clears all in-progress editing/recording sub-state for the current submenu item -- used once a value has been
    * submitted, a conflict resolved, or a recording finished. Always rebuilds the current page as a Group; there is no
    * separate recording sub-state left to clear once the page is rebuilt this way, since `Editing.recording` only
    * exists on an `Editing` page.
    */
  def clearSubmenuEditingAndRecording: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        withDrilledSettingsSurface(
          surface.copy(current =
            SettingsPage.Group(surface.current.groupId, pageSelectedIndex(surface.current), surface.current.searchTerm)
          )
        )
      case None =>
        this

  /** Begins recording a keybinding for `itemId`: an edit with empty draft text, tagged with a fresh `RecordingState`.
    */
  def beginSubmenuRecording(itemId: String): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        withDrilledSettingsSurface(
          surface.copy(current =
            SettingsPage.Editing(
              groupId = surface.current.groupId,
              itemId = itemId,
              draftText = "",
              searchTerm = surface.current.searchTerm,
              recording = Some(RecordingState(itemId))
            )
          )
        )
      case None =>
        this

  /** Stashes a just-recorded keystroke as pending, awaiting a possible double-tap within the recorder's time window. */
  def withPendingRecordedBinding(info: KeyStrokeInfo, recordedAtMillis: Long): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        surface.current match
          case editing: SettingsPage.Editing =>
            val recording = editing.recording.getOrElse(RecordingState(editing.itemId))
            withDrilledSettingsSurface(
              surface.copy(current =
                editing.copy(recording = Some(recording.copy(pendingRecordedBinding = Some(info -> recordedAtMillis))))
              )
            )
          case _ =>
            this
      case None =>
        this

  /** Deletes one character from the current settings page's text -- an in-progress edit's draft, or (when not editing)
    * a group's local search -- via `SettingsSurfaceState.deleteBackward`. A no-op when there is no text to delete;
    * never navigates the stack. This replaces Backspace's old fallback to `exitSubmenuToPreview` once text was already
    * empty (issue #1059).
    */
  def deleteSubmenuTextBackward: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val updated = SettingsSurfaceState.deleteBackward(surface)
        if updated == surface then this
        else withDrilledSettingsSurface(updated).copy(statusMessage = None)
      case None =>
        this

  def enterSelectedSubmenuGroup: CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        val items   = filteredPageItems(surface.current, submenuItems(groupId))
        items.lift(pageSelectedIndex(surface.current)) match
          case Some(group: CommandSurfaceItem.GroupItem) =>
            val rememberedIndex = submenuSelections.getOrElse(group.id, 0)
            copy(
              submenuSelections = submenuSelections + (groupId -> pageSelectedIndex(surface.current)),
              // A true push: the group we're leaving becomes the nearest ancestor of the group we're entering.
              surface = CommandRunnerSurface.Settings(
                root = rootState,
                drilled = Some(surface.push(SettingsPage.Group(group.id, rememberedIndex)))
              )
            )
          case _ =>
            this
      case None =>
        this

  /** Adjusting an option's value writes to `optionSelections`, not to the page-stack itself, so `activeSettingsSurface`
    * is left exactly as it was.
    */
  def adjustSelectedSubmenuOption(delta: Int): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val items = filteredPageItems(surface.current, submenuItems(surface.current.groupId))
        items.lift(pageSelectedIndex(surface.current)) match
          case Some(option: CommandSurfaceItem.OptionItem) =>
            val updatedOption = option.moveSelection(delta)
            copy(optionSelections = optionSelections + (option.id -> updatedOption.selectedIndex))
          case _ =>
            this
      case None =>
        this

  def adjustSelectedOption(delta: Int): CommandRunner =
    selectedItem match
      case Some(option: CommandSurfaceItem.OptionItem) =>
        val updatedOption = option.moveSelection(delta)
        copy(optionSelections = optionSelections + (option.id -> updatedOption.selectedIndex))
      case _ =>
        this

  def withSelectedItem(itemId: String): CommandRunner =
    visibleItems.zipWithIndex.find(_._1.id == itemId) match
      case Some((_, index)) => withRootSelectedIndex(index).syncEditMode
      case None             => this

  def withSelectedVisibleIndex(index: Int): CommandRunner =
    if visibleItems.indices.contains(index) then withRootSelectedIndex(index).syncEditMode
    else this

  def withSelectedFocusedSubmenuIndex(index: Int): CommandRunner =
    activeSettingsSurface match
      case Some(surface) =>
        val groupId = surface.current.groupId
        val items   = filteredPageItems(surface.current, submenuItems(groupId))
        if items.indices.contains(index) then
          copy(
            submenuSelections = submenuSelections + (groupId -> index),
            // As with moveSubmenuSelection: setting an index directly always exits edit mode, so this is always
            // rebuilt as a Group.
            surface = CommandRunnerSurface.Settings(
              root = rootState,
              drilled = Some(surface.copy(current = SettingsPage.Group(groupId, index, surface.current.searchTerm)))
            )
          )
        else this
      case None =>
        this
