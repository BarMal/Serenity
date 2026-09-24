package com.serenity.state.manager

import com.serenity.command.{CommandRunner, SettingsPage, SettingsSurfaceState}
import com.serenity.config.{AppConfig, DefaultDocumentMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.RichTextDocument
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.presets.{UiPreset, UiPresetDiff}

/** What a preset operation reports on the command runner once it has finished. */
private[manager] enum UiPresetContext:
  case Status(presetName: Option[String], message: String)

  /** Focuses the just-created preset's own editing group (#1059: on the one `CommandPalette` surface, like every other
    * settings drill-in, rather than a second floating one).
    */
  case CreatedPresetFocused(presetName: String, message: String)

/** How a requested preset apply ended once its preset and theme were loaded off the dispatcher. */
private[manager] enum UiPresetApplyResolution:
  case Apply(preset: UiPreset, restore: AppState => AppState)
  case Reject(presetName: String, message: String)
  case Abandon

/** The pure state side of the UI-preset effects (#1697): every preset write to state goes through one of these and is
  * committed through `AppStateValidation`.
  */
private[manager] object UiPresetTransitions:

  def withFeedback(state: AppState, previews: Option[List[UiPreset.Preview]], context: UiPresetContext): AppState =
    val withPreviews = previews.fold(state)(list => updateCommandRunner(state)(_.withUiPresetPreviews(list)))
    context match
      case UiPresetContext.Status(presetName, message) =>
        withPresetContext(withPreviews, presetName, message)
      case UiPresetContext.CreatedPresetFocused(presetName, message) =>
        withCreatedPresetFocused(withPreviews, presetName, message)

  def withPresetContext(state: AppState, presetName: Option[String], message: String): AppState =
    updateCommandRunner(state)(
      _.copy(editingPresetName = presetName, editingItemId = None, editingText = "", statusMessage = Some(message))
    )

  private def withCreatedPresetFocused(state: AppState, presetName: String, message: String): AppState =
    state.commandRunnerSurface.fold(state) { surface =>
      val focused = updateCommandRunner(state)(runner =>
        runner
          .withDrilledSettingsSurface(
            SettingsSurfaceState(
              SettingsPage.Group("settings-preset-edit"),
              List(SettingsPage.Group("settings-ui-presets", 2))
            )
          )
          .copy(
            submenuSelections = runner.submenuSelections + ("settings-ui-presets" -> 2),
            editingItemId = None,
            editingText = "",
            editingPresetName = Some(presetName.trim),
            statusMessage = Some(message)
          )
      )
      if focused eq state then state
      else focused.copy(persisted = focused.persisted.copy(focus = Focus.Surface(surface.id)))
    }

  def openDiffReview(state: AppState, preset: UiPreset): AppState =
    val changes = UiPresetDiff.changes(
      currentConfig = state.persisted.config,
      currentThemeName = state.persisted.theme.name,
      currentHasDockedPanels = state.pinnedSurfaces.nonEmpty,
      currentHasWorkspaceTree = state.persisted.layout.workspaceTree.isDefined,
      preset = preset
    )
    updateCommandRunner(ensureCommandRunnerSurfaceForReview(state))(_.openPresetDiffReview(preset.name, changes))

  /** A bare, minimally-activated command-runner surface to host the review when none is already open -- unlike
    * `StateManagerOperationBoundary.ensureCommandRunnerSurface`, this needs no `CommandRegistry`: the review's items
    * come entirely from `CommandRunner.openPresetDiffReview`, not the palette's command list.
    */
  private def ensureCommandRunnerSurfaceForReview(state: AppState): AppState =
    state.commandRunnerSurface match
      case Some(_) => state
      case None =>
        val (stateWithId, surfaceId) = state.allocateSurfaceId
        val surface = UiSurface(
          id = surfaceId,
          content = SurfaceContent.CommandPalette(CommandRunner.empty.copy(isActive = true)),
          presentation = SurfacePresentation.Floating(stateWithId.activeCursorPosition, SurfacePlacement.BelowCursor)
        )
        stateWithId
          .copy(runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ surface))
          .pushFocus(Focus.Surface(surfaceId))

  /** Records a new apply request, numbered after the one still pending so a result can tell it has been superseded.
    * Numbers restart once nothing is pending: the Presets lane resolves requests in order, so no older result can still
    * be on its way by then.
    */
  def requestApply(state: AppState): (AppState, Long) =
    val request = state.runtime.pendingUiPresetApply.fold(1L)(_ + 1)
    (state.copy(runtime = state.runtime.copy(pendingUiPresetApply = Some(request))), request)

  def resolveApply(state: AppState, request: Long, resolution: UiPresetApplyResolution): AppState =
    if !state.runtime.pendingUiPresetApply.contains(request) then state
    else
      val settled = state.copy(runtime = state.runtime.copy(pendingUiPresetApply = None))
      resolution match
        case UiPresetApplyResolution.Apply(_, restore)          => restore(settled)
        case UiPresetApplyResolution.Reject(presetName, reason) => withPresetContext(settled, Some(presetName), reason)
        case UiPresetApplyResolution.Abandon                    => settled

  /** The full state change of applying a loaded preset: `restore` is the one step that differs between the one-shot
    * apply and the review's selective one.
    */
  def restored(
    preset: UiPreset,
    restore: AppState => AppState,
    withUpdatedRunnerConfig: (AppState, AppConfig) => AppState
  )(state: AppState)(using com.serenity.rope.Balance): AppState =
    // From the splash there is no editor pane/buffer/tree to apply onto, so seed a fresh "New document" workspace
    // first (dropping the splash) and apply the preset on top -- the same valid base a runtime preset-apply sees
    // (#1524 and its buffer-less-pane fallout).
    val restoredPresetState = restore(seedEditorFromSplash(state))
    val restoredDocumentState =
      applyPresetDocumentModeToActiveEmptyBuffer(restoredPresetState, preset.config.defaultDocumentMode)
    val restoredOutlineState = hydratePresetSymbolPanels(restoredDocumentState)
    withUpdatedRunnerConfig(restoredOutlineState, restoredOutlineState.persisted.config)

  /** Choosing a workflow preset from the startup splash must leave the splash and land in a usable editor (#1524): the
    * splash has no editor pane, buffer, or workspace tree, so a preset applied onto it directly produced a buffer-less
    * pane, or a tree with no editor leaf that failed validation. A no-op at runtime (no splash).
    */
  private def seedEditorFromSplash(state: AppState)(using com.serenity.rope.Balance): AppState =
    if state.startPageSurface.isEmpty then state
    else
      val withoutStartPage =
        state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot { surface =>
          surface.content match
            case SurfaceContent.StartPage(_) => true
            case _                           => false
        }))
      EditorState.openNewTab(withoutStartPage)

  private def applyPresetDocumentModeToActiveEmptyBuffer(state: AppState, mode: DefaultDocumentMode): AppState =
    state.focusedBufferId.flatMap(state.persisted.buffers.get) match
      case Some(buffer)
          if buffer.document.isNewEmpty && buffer.document.content.weight == 0 && buffer.document.filePath.isEmpty =>
        val updatedBuffer =
          mode match
            case DefaultDocumentMode.PlainText =>
              buffer.copy(
                document = buffer.document.copy(language = None),
                richText = buffer.richText.copy(richTextDocument = None)
              )
            case DefaultDocumentMode.Markdown =>
              buffer.copy(
                document = buffer.document.copy(language = Some(LanguageId.Markdown)),
                richText = buffer.richText.copy(richTextDocument = None)
              )
            case DefaultDocumentMode.RichText =>
              buffer.copy(
                document = buffer.document.copy(language = None),
                richText = buffer.richText.copy(richTextDocument = Some(RichTextDocument.fromPlainText("")))
              )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> updatedBuffer)))
      case _ =>
        state

  private def hydratePresetSymbolPanels(state: AppState): AppState =
    val outlineSymbolsList = PanelSymbolLookup.outlineSymbols(state)
    val outlineActive      = PanelSymbolLookup.currentSymbolActiveLocation(outlineSymbolsList, state)
    val commentSymbolsList = PanelSymbolLookup.commentPanelSymbols(state)
    val commentActive      = PanelSymbolLookup.currentSymbolActiveLocation(commentSymbolsList, state)
    val hydratedSurfaces = state.runtime.uiSurfaces.map {
      case surface @ UiSurface(_, SurfaceContent.Outline(_, _), SurfacePresentation.Docked, _) =>
        surface.copy(content = SurfaceContent.Outline(outlineSymbolsList, outlineActive))
      case surface @ UiSurface(_, SurfaceContent.Comments(_, _), SurfacePresentation.Docked, _) =>
        surface.copy(content = SurfaceContent.Comments(commentSymbolsList, commentActive))
      case surface =>
        surface
    }
    state.copy(runtime = state.runtime.copy(uiSurfaces = hydratedSurfaces))

  private def updateCommandRunner(state: AppState)(f: CommandRunner => CommandRunner): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val updatedSurfaces = state.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(
              _.copy(content = SurfaceContent.CommandPalette(f(runner)))
            )
            state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
          case _ =>
            state
      case None =>
        state
