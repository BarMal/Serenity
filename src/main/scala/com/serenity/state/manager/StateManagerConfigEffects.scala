package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.animation.AnimationConfig
import com.serenity.animation.sprite.CompanionSpriteConfig
import com.serenity.command.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, CursorInfoBarSegment, VisualFlairLevel}
import com.serenity.session.{SessionPersistence, SessionSaveTrigger}
import com.serenity.state.models.*
import com.serenity.state.reducers.{CommandRunnerPanelSelections, CommandRunnerReducer}

/** Config-update infrastructure and the settings-intent dispatch that drives it: appearance, motion, cursor, panel
  * chrome, spell-check, and general settings all funnel through the same persist-then-auto-save path.
  */
final private[manager] class StateManagerConfigEffects(
    stateRef: Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    configPersistencePath: Option[java.nio.file.Path],
    sessionPersistence: SessionPersistence,
    bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]],
    onFontConfigChanged: com.serenity.ui.fonts.FontLoader.FontConfig => IO[Unit],
    deviceTextScaleProvider: IO[Double],
    editor: EffectEditorPort
)(using balance: com.serenity.rope.Balance):

  private[manager] def withUpdatedRunnerConfig(state: AppState, config: AppConfig): AppState =
    val commandRunnerSurfaceId = state.commandRunnerSurface.map(_.id)
    val updatedRunner =
      state.commandRunnerSurface.flatMap { surface =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val configRunner = runner.updateInputItems(config)
            Some(
              configRunner.copy(optionSelections =
                configRunner.optionSelections ++ CommandRunnerPanelSelections.fromState(state)
              )
            )
          case _ =>
            None
      }
    val updatedSurfaces = state.runtime.uiSurfaces.map {
      // When updatedRunner is None, .fold falls back to the surface unchanged -- the same outcome the
      // original `if updatedRunner.isDefined` guard produced by skipping to the `case other => other` tail.
      case current if commandRunnerSurfaceId.contains(current.id) =>
        updatedRunner.fold(current)(runner => current.copy(content = SurfaceContent.CommandPalette(runner)))
      case current @ UiSurface(_, SurfaceContent.ContextualToolbar(toolbarState), _, _) =>
        current.copy(
          content = SurfaceContent.ContextualToolbar(
            toolbarState.copy(displayMode = config.surfaceConfig.contextualToolbarDisplayMode)
          )
        )
      case other =>
        other
    }
    state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))

  private[manager] def updateConfig(update: AppConfig => AppConfig): IO[AppConfig] =
    applyConfigUpdate(update)

  private def updateAppearanceConfig(update: AppConfig => AppConfig): IO[AppConfig] =
    applyConfigUpdate(update)

  private def updateMotionConfig(update: AppConfig => AppConfig): IO[AppConfig] =
    stateRef.get.flatMap { previousState =>
      applyConfigUpdate(update).flatTap(cancelDisabledMotion(previousState.persisted.config, _))
    }

  private def updateMotionAccessibility(accessibility: com.serenity.config.MotionAccessibility): IO[AppConfig] =
    updateMotionConfig(_.withMotionAccessibility(accessibility))

  private def updateWindowSitterConfig(
    update: com.serenity.animation.WindowSitterConfig => com.serenity.animation.WindowSitterConfig
  ): IO[Unit] =
    updateAppearanceConfig(config => config.withWindowSitterConfig(update(config.windowSitterConfig))).flatTap { config =>
      stateRef.update { state =>
        val sitter =
          if config.windowSitterConfig.enabled then
            com.serenity.animation.WindowSitter.fromConfig(config.windowSitterConfig)
          else com.serenity.animation.WindowSitter.default
        state.copy(runtime = state.runtime.copy(windowSitter = sitter))
      }
    }.void

  private def updateCompanionSpriteConfig(update: CompanionSpriteConfig => CompanionSpriteConfig): IO[Unit] =
    updateAppearanceConfig(config => config.withCompanionSpriteConfig(update(config.companionSpriteConfig)))
      .flatTap(config => stateRef.update(state => syncCompanionSpritePanel(state, config)))
      .void

  private def updateVisualFlairLevel(level: VisualFlairLevel): IO[Unit] =
    updateAppearanceConfig(_.withVisualFlairLevel(level))
      .flatTap(config => stateRef.update(state => syncCompanionSpritePanel(state, config)))
      .void

  /** Adds or removes the companion sprite's pinned panel surface to match the config: visible exactly when the
    * companion sprite is enabled and visual flair is not `Off` (matching item 8/9's "Off = don't render" rule). Called
    * after either setting changes, since either can flip the panel's visibility.
    */
  private def syncCompanionSpritePanel(state: AppState, config: AppConfig): AppState =
    val shouldShow = config.companionSpriteConfig.enabled && config.visualFlairLevel != VisualFlairLevel.Off
    val exists     = state.runtime.uiSurfaces.exists(_.id == SurfaceId.CompanionSprite)
    if shouldShow && !exists then
      val surface = UiSurface(
        id = SurfaceId.CompanionSprite,
        content = SurfaceContent.CompanionSprite,
        presentation = SurfacePresentation.Docked
      )
      // Docking is the tree's job alone (issue #817) -- `AppState.dockCompanionSprite` is the one place that seeds a
      // companion sprite's position/ratio, reused here so startup and this runtime toggle can't drift apart.
      state.copy(
        persisted = state.persisted.copy(layout = AppState.dockCompanionSprite(state.persisted.layout, config)),
        runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces :+ surface)
      )
    else if !shouldShow && exists then
      val prunedTree = state.persisted.layout.workspaceTree.flatMap(_.removeSurface(SurfaceId.CompanionSprite))
      val maximized = state.persisted.layout.maximizedWorkspaceNodeId.filterNot(nodeId =>
        state.persisted.layout.workspaceTree.flatMap(_.surfaceIdForNode(nodeId)).contains(SurfaceId.CompanionSprite)
      )
      state.copy(
        persisted = state.persisted.copy(layout =
          state.persisted.layout.copy(
            workspaceTree = prunedTree.orElse(state.persisted.layout.workspaceTree),
            maximizedWorkspaceNodeId = maximized
          )
        ),
        runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == SurfaceId.CompanionSprite))
      )
    else state

  private def cancelActiveMotion(): IO[Unit] =
    clearBufferAnimations() >>
      stateRef.update(state =>
        state.copy(runtime =
          state.runtime.copy(
            themeTransition = None,
            uiSurfaces = state.runtime.uiSurfaces.filterNot(isGhostOverlay),
            surfaceAnimations = Map.empty,
            windowSitter = com.serenity.animation.WindowSitter.default
          )
        )
      )

  private def cancelDisabledMotion(previous: AppConfig, current: AppConfig): IO[Unit] =
    val previousFamilies = previous.surfaceConfig.effectiveMotionConfiguration
    val currentFamilies  = current.surfaceConfig.effectiveMotionConfiguration
    if currentFamilies.families.values.forall(!_.enabled) && previousFamilies.families.values.exists(_.enabled) then
      cancelActiveMotion()
    else
      com.serenity.config.MotionFamily.values.toList
        .filter(family => previousFamilies.family(family).enabled && !currentFamilies.family(family).enabled)
        .traverse_(cancelMotionFamily) >>
        IO.whenA(
          !previousFamilies.family(com.serenity.config.MotionFamily.UiTransitions).enabled &&
            currentFamilies.family(com.serenity.config.MotionFamily.UiTransitions).enabled &&
            current.windowSitterConfig.enabled
        )(
          stateRef.update(state =>
            state.copy(runtime =
              state.runtime.copy(windowSitter =
                com.serenity.animation.WindowSitter.fromConfig(current.windowSitterConfig)
              )
            )
          )
        )

  private def cancelMotionFamily(family: com.serenity.config.MotionFamily): IO[Unit] =
    family match
      case com.serenity.config.MotionFamily.EditorText =>
        clearBufferAnimations(com.serenity.animation.AnimationOwner.EditorText)
      case com.serenity.config.MotionFamily.CommandSurfaces =>
        cancelSurfaceMotion(isCommandSurface)
      case com.serenity.config.MotionFamily.PinnedPanels =>
        cancelSurfaceMotion(isDockedSurface)
      case com.serenity.config.MotionFamily.UiTransitions =>
        clearBufferAnimations(com.serenity.animation.AnimationOwner.UiTransitions) >>
          stateRef.update(state =>
            state.copy(runtime =
              state.runtime.copy(
                themeTransition = None,
                windowSitter = com.serenity.animation.WindowSitter.default
              )
            )
          )
      case com.serenity.config.MotionFamily.Cursor =>
        IO.unit

  private def clearBufferAnimations(): IO[Unit] =
    bufferAnimationsRef.update(
      _.view
        .mapValues { animations =>
          val cleared = animations.clearAll()
          if cleared eq animations then animations else cleared
        }
        .toMap
    )

  private def clearBufferAnimations(owner: com.serenity.animation.AnimationOwner): IO[Unit] =
    bufferAnimationsRef.update(
      _.view
        .mapValues { animations =>
          val cleared = animations.clear(owner)
          if cleared eq animations then animations else cleared
        }
        .toMap
    )

  private def cancelSurfaceMotion(matches: UiSurface => Boolean): IO[Unit] =
    stateRef.update { state =>
      val matchingIds = state.runtime.uiSurfaces.collect { case surface if matches(surface) => surface.id }.toSet
      state.copy(runtime =
        state.runtime.copy(
          uiSurfaces = state.runtime.uiSurfaces.filterNot(surface => matches(surface) && isGhostOverlay(surface)),
          surfaceAnimations =
            state.runtime.surfaceAnimations.filterNot((surfaceId, _) => matchingIds.contains(surfaceId))
        )
      )
    }

  private def isCommandSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.CommandPalette(_) => true
      case SurfaceContent.GhostOverlay(content, _) =>
        content match
          case SurfaceContent.CommandPalette(_) => true
          case _                                => false
      case _ => false

  /** A transient close-fade ghost, which is discarded rather than animated when motion is cancelled. */
  private def isGhostOverlay(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.GhostOverlay(_, _) => true
      case _                                 => false

  /** A surface occupying a workspace dock, whether at its pinned size or expanded over the editor. */
  private def isDockedSurface(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Docked => true
      case _                          => false

  private def updateCustomMotionConfig(update: AppConfig => AppConfig): IO[AppConfig] =
    updateMotionConfig(config => update(config).withCustomMotionBaseline)

  private def updateTextDisplayConfig(update: AppConfig => AppConfig): IO[AppConfig] =
    applyConfigUpdate(update)

  /** Applies a configuration change to live state, persists it, and auto-saves the session. */
  private def applyConfigUpdate(update: AppConfig => AppConfig): IO[AppConfig] =
    stateRef
      .modify { state =>
        val newConfig = update(state.persisted.config)
        val newState =
          withUpdatedRunnerConfig(state.copy(persisted = state.persisted.copy(config = newConfig)), newConfig)
        (newState, newConfig)
      }
      .flatTap(config =>
        configPersistencePath match
          case Some(path) =>
            com.serenity.config.ConfigManager.saveConfigIO(config, path).flatMap {
              case Right(_) => IO.unit
              case Left(error) =>
                logger
                  .warn(error.cause.getOrElse(new RuntimeException(error.message)))(s"[CONFIG] ${error.message}")
            }
          case None =>
            IO.unit
      )
      .flatTap(_ =>
        stateRef.get
          .flatMap(state => sessionPersistence.maybeSaveSession(state, SessionSaveTrigger.Manual))
          .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after config change failed"))
      )

  private[manager] def updateFontConfig(
    update: com.serenity.ui.fonts.FontLoader.FontConfig => com.serenity.ui.fonts.FontLoader.FontConfig
  ): IO[Unit] =
    deviceTextScaleProvider.flatMap { deviceTextScale =>
      applyConfigUpdate(config =>
        config.withFontConfig(update(config.editorConfig.fontConfig).resolveAutoTextScale(deviceTextScale))
      )
        .flatMap(config => onFontConfigChanged(config.editorConfig.fontConfig))
    }

  private def updateSpellCheckConfig(
    update: com.serenity.config.SpellCheckConfig => com.serenity.config.SpellCheckConfig
  ): IO[Unit] =
    applyConfigUpdate(config => config.withSpellCheck(update(config.languageToolsConfig.spellCheck))).void >>
      editor.scheduleDocumentAnalysis()

  private def clampFontSize(size: Float): Float =
    size.max(8.0f).min(48.0f)

  private[manager] def interpret(intent: SettingsIntent, state: AppState): IO[Unit] =
    intent match
      case SettingsIntent.Font(fontIntent)               => interpretFontIntent(fontIntent)
      case SettingsIntent.Motion(motionIntent)           => interpretMotionIntent(motionIntent)
      case SettingsIntent.Cursor(cursorIntent)           => interpretCursorIntent(cursorIntent)
      case SettingsIntent.PanelChrome(panelChromeIntent) => interpretPanelChromeIntent(panelChromeIntent)
      case SettingsIntent.SpellCheck(spellCheckIntent)   => interpretSpellCheckIntent(spellCheckIntent)
      case SettingsIntent.General(generalIntent)         => interpretGeneralSettingsIntent(generalIntent, state)

  private def interpretFontIntent(intent: FontIntent): IO[Unit] =
    intent match
      case FontIntent.IncreaseFontSize =>
        updateFontConfig(config =>
          config.copy(
            fontSize = clampFontSize(config.fontSize + 1.0f),
            textFontSize = clampFontSize(config.textFontSize + 1.0f)
          )
        )
      case FontIntent.DecreaseFontSize =>
        updateFontConfig(config =>
          config.copy(
            fontSize = clampFontSize(config.fontSize - 1.0f),
            textFontSize = clampFontSize(config.textFontSize - 1.0f)
          )
        )
      case FontIntent.SetFontSize(size) =>
        updateFontConfig(config => config.copy(fontSize = clampFontSize(size), textFontSize = clampFontSize(size)))
      case FontIntent.SetCodeFontSize(size) =>
        updateFontConfig(_.copy(fontSize = clampFontSize(size)))
      case FontIntent.SetTextFontSize(size) =>
        updateFontConfig(_.copy(textFontSize = clampFontSize(size)))
      case FontIntent.SetUiFontSize(size) =>
        updateFontConfig(_.copy(uiFontSize = clampFontSize(size)))
      case FontIntent.SetTextScaleMode(mode) =>
        updateFontConfig(_.copy(textScaleMode = mode))
      case FontIntent.SetTextScaleMultiplier(scale) =>
        updateFontConfig(config =>
          config.copy(
            textScaleMode = com.serenity.ui.fonts.FontLoader.TextScaleMode.Manual,
            textScaleMultiplier = com.serenity.ui.fonts.FontLoader.FontConfig.clampTextScale(scale)
          )
        )
      case FontIntent.SetCodeFontFamily(family) =>
        updateFontConfig(_.copy(codeFontFamily = family))
      case FontIntent.SetTextFontFamily(family) =>
        updateFontConfig(_.copy(textFontFamily = family))
      case FontIntent.SetUiFontFamily(family) =>
        updateFontConfig(_.copy(uiFontFamily = family))
      case FontIntent.SetLigatures(enabled) =>
        updateFontConfig(_.copy(enableLigatures = enabled, textLigatures = enabled))
      case FontIntent.SetCodeLigatures(enabled) =>
        updateFontConfig(_.copy(enableLigatures = enabled))
      case FontIntent.SetTextLigatures(enabled) =>
        updateFontConfig(_.copy(textLigatures = enabled))
      case FontIntent.SetUiLigatures(enabled) =>
        updateFontConfig(_.copy(uiLigatures = enabled))
      case FontIntent.ToggleLigatures =>
        updateFontConfig(config =>
          config.copy(enableLigatures = !config.enableLigatures, textLigatures = !config.textLigatures)
        )

  private def interpretMotionIntent(intent: MotionIntent): IO[Unit] =
    intent match
      case MotionIntent.SetMotionPreset(preset) =>
        updateMotionConfig(_.withMotionPreset(preset)).void
      case MotionIntent.SetMotionAccessibility(accessibility) =>
        updateMotionAccessibility(accessibility).void
      case MotionIntent.SetElementTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withElementTransitionSpeedScale(scale)).void
      case MotionIntent.SetEditorTextTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withEditorTextTransitionSpeedScale(Some(scale))).void
      case MotionIntent.SetCommandRunnerTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withCommandRunnerTransitionSpeedScale(Some(scale))).void
      case MotionIntent.SetUiTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withUiTransitionSpeedScale(Some(scale))).void
      case MotionIntent.SetCursorTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withCursorTransitionSpeedScale(Some(scale))).void
      case MotionIntent.SetCommandRunnerAnimation(animation) =>
        updateCustomMotionConfig(_.withCommandRunnerAnimation(animation)).void
      case MotionIntent.SetUiAnimation(animation) =>
        updateCustomMotionConfig(_.withUiAnimation(animation)).void
      case MotionIntent.SetCommandRunnerVisibleRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerVisibleRows(rows)).void
      case MotionIntent.SetCommandRunnerItemGapRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerItemGapRows(rows)).void
      case MotionIntent.SetCommandRunnerCursorGapRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerCursorGapRows(rows)).void
      case MotionIntent.SetEditorInsertionTransitionKind(kind) =>
        updateCustomMotionConfig(_.withEditorInsertionTransitionKind(kind)).void
      case MotionIntent.SetCommandRunnerTransitionKind(kind) =>
        updateCustomMotionConfig(_.withCommandRunnerTransitionKind(Some(kind))).void
      case MotionIntent.SetPanelOpenTransitionKind(kind) =>
        updateCustomMotionConfig(_.withPanelOpenTransitionKind(Some(kind))).void
      case MotionIntent.SetPanelCloseTransitionKind(kind) =>
        updateCustomMotionConfig(_.withPanelCloseTransitionKind(Some(kind))).void

  private def interpretCursorIntent(intent: CursorIntent): IO[Unit] =
    intent match
      case CursorIntent.SetCursorMode(mode) =>
        updateAppearanceConfig(_.withCursorMode(mode)).void
      case CursorIntent.SetCursorInfoBarSegmentIncluded(segment, included) =>
        updateAppearanceConfig { config =>
          val current = config.cursorInfoBarSegments
          val updated =
            if included then if current.contains(segment) then current else current :+ segment
            else current.filterNot(_ == segment)
          config.withCursorInfoBarSegments(updated)
        }.void
      case CursorIntent.MoveCursorInfoBarSegmentEarlier(segment) =>
        updateAppearanceConfig(config =>
          config.withCursorInfoBarSegments(moveCursorInfoBarSegment(config.cursorInfoBarSegments, segment, -1))
        ).void
      case CursorIntent.MoveCursorInfoBarSegmentLater(segment) =>
        updateAppearanceConfig(config =>
          config.withCursorInfoBarSegments(moveCursorInfoBarSegment(config.cursorInfoBarSegments, segment, 1))
        ).void
      case CursorIntent.SetCursorInfoBarPlacement(placement) =>
        updateAppearanceConfig(_.withCursorInfoBarPlacement(placement)).void

  private def moveCursorInfoBarSegment(
    segments: List[CursorInfoBarSegment],
    segment: CursorInfoBarSegment,
    delta: Int
  ): List[CursorInfoBarSegment] =
    val index  = segments.indexOf(segment)
    val target = index + delta
    if index < 0 || target < 0 || target >= segments.length then segments
    else
      segments.zipWithIndex.map {
        case (_, `index`)  => segments(target)
        case (_, `target`) => segments(index)
        case (other, _)    => other
      }

  private def interpretPanelChromeIntent(intent: PanelChromeIntent): IO[Unit] =
    intent match
      case PanelChromeIntent.ToggleLineNumbers =>
        updateTextDisplayConfig(config => config.withLineNumbers(!config.surfaceConfig.showLineNumbers)).void
      case PanelChromeIntent.ToggleGutter =>
        updateTextDisplayConfig(config => config.withGutter(!config.surfaceConfig.showGutter)).void
      case PanelChromeIntent.ToggleWordWrap =>
        updateTextDisplayConfig(config => config.withWordWrap(!config.surfaceConfig.wordWrapEnabled)).void
      case PanelChromeIntent.ToggleFocusedTextBody =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(!config.surfaceConfig.focusedTextBodyEnabled)).void
      case PanelChromeIntent.ToggleContextualToolbar =>
        editor.enqueueEvent(com.serenity.keystroke.events.ToggleContextualToolbar)
      case PanelChromeIntent.TogglePaneHeaders =>
        updateTextDisplayConfig(config => config.withPaneHeaders(!config.surfaceConfig.showPaneHeaders)).void
      case PanelChromeIntent.ToggleVisualLineCursorNavigation =>
        updateTextDisplayConfig(config =>
          config.withVisualLineCursorNavigation(!config.surfaceConfig.visualLineCursorNavigation)
        ).void
      case PanelChromeIntent.ToggleTypewriterScrolling =>
        updateTextDisplayConfig(config =>
          config.withTypewriterScrolling(!config.surfaceConfig.typewriterScrollingEnabled)
        ).void
      case PanelChromeIntent.SetLineNumbers(enabled) =>
        updateTextDisplayConfig(config => config.withLineNumbers(enabled)).void
      case PanelChromeIntent.SetGutter(enabled) =>
        updateTextDisplayConfig(config => config.withGutter(enabled)).void
      case PanelChromeIntent.SetWordWrap(enabled) =>
        updateTextDisplayConfig(config => config.withWordWrap(enabled)).void
      case PanelChromeIntent.SetVisualLineCursorNavigation(enabled) =>
        updateTextDisplayConfig(config => config.withVisualLineCursorNavigation(enabled)).void
      case PanelChromeIntent.SetTypewriterScrolling(enabled) =>
        updateTextDisplayConfig(config => config.withTypewriterScrolling(enabled)).void
      case PanelChromeIntent.SetFocusedTextBody(enabled) =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(enabled)).void
      case PanelChromeIntent.SetContextualToolbarEnabled(enabled) =>
        updateTextDisplayConfig(config => config.withContextualToolbarEnabled(enabled)).void
      case PanelChromeIntent.SetContextualToolbarDisplayMode(mode) =>
        updateTextDisplayConfig(config => config.withContextualToolbarDisplayMode(mode)).void
      case PanelChromeIntent.SetCommandRunnerShowKeyHints(enabled) =>
        updateAppearanceConfig(_.withCommandRunnerShowKeyHints(enabled)).void
      case PanelChromeIntent.SetUiElementGap(gap) =>
        updateAppearanceConfig(_.withUiElementGap(gap)).void
      case PanelChromeIntent.SetUiCornerRadiusPx(radius) =>
        updateAppearanceConfig(_.withUiCornerRadiusPx(radius)).void
      case PanelChromeIntent.SetUiOutlineThicknessPx(thickness) =>
        updateAppearanceConfig(_.withUiOutlineThicknessPx(thickness)).void
      case PanelChromeIntent.SetInterfaceDensity(density) =>
        updateAppearanceConfig(_.withInterfaceDensity(density)).void
      case PanelChromeIntent.SetWindowChromeMode(mode) =>
        updateAppearanceConfig(_.withWindowChromeMode(mode)).void
      case PanelChromeIntent.SetWindowSitterEnabled(enabled) =>
        updateWindowSitterConfig(_.copy(enabled = enabled))
      case PanelChromeIntent.SetWindowSitterAction(action) =>
        updateWindowSitterConfig(_.copy(action = action))
      case PanelChromeIntent.SetWindowSitterFrames(frames) =>
        updateWindowSitterConfig(_.copy(frames = frames))
      case PanelChromeIntent.SetWindowSitterActiveTicks(ticks) =>
        updateWindowSitterConfig(_.copy(activeTicks = ticks))
      case PanelChromeIntent.SetWindowSitterFastActiveTicks(ticks) =>
        updateWindowSitterConfig(_.copy(fastActiveTicks = ticks))
      case PanelChromeIntent.SetWindowSitterFastTypingThresholdMs(ms) =>
        updateWindowSitterConfig(_.copy(fastTypingThresholdMs = ms))
      case PanelChromeIntent.SetCompanionSpriteEnabled(enabled) =>
        updateCompanionSpriteConfig(_.copy(enabled = enabled))
      case PanelChromeIntent.SetVisualFlairLevel(level) =>
        updateVisualFlairLevel(level)
      case PanelChromeIntent.SetWheelScrollLines(lines) =>
        updateConfig(_.withWheelScrollLines(lines)).void
      case PanelChromeIntent.SetTextAreaLeftInset(value) =>
        updateTextDisplayConfig(_.withTextAreaLeftInset(value)).void
      case PanelChromeIntent.SetTextAreaRightInset(value) =>
        updateTextDisplayConfig(_.withTextAreaRightInset(value)).void
      case PanelChromeIntent.SetTextAreaTopInset(value) =>
        updateTextDisplayConfig(_.withTextAreaTopInset(value)).void
      case PanelChromeIntent.SetTextAreaBottomInset(value) =>
        updateTextDisplayConfig(_.withTextAreaBottomInset(value)).void
      case PanelChromeIntent.SetShowWordCount(enabled) =>
        updateTextDisplayConfig(_.withWordCount(enabled)).void

  private def interpretSpellCheckIntent(intent: SpellCheckIntent): IO[Unit] =
    intent match
      case SpellCheckIntent.SetSpellCheckEnabled(enabled) =>
        updateSpellCheckConfig(_.copy(enabled = enabled))
      case SpellCheckIntent.SetSpellCheckLanguages(languages) =>
        updateSpellCheckConfig(_.copy(languages = languages))
      case SpellCheckIntent.SetSpellCheckDictionaryPaths(paths) =>
        updateSpellCheckConfig(_.copy(dictionaryPaths = paths))
      case SpellCheckIntent.SetSpellCheckWords(words) =>
        updateSpellCheckConfig(_.copy(additionalWords = words))

  private def interpretGeneralSettingsIntent(intent: GeneralSettingsIntent, state: AppState): IO[Unit] =
    intent match
      case GeneralSettingsIntent.OpenSettings =>
        editor.updateState(current =>
          CommandRunnerReducer.openSettings(current, CommandRegistry.withToggleUI)(using balance)
        )
      case GeneralSettingsIntent.SaveConfig =>
        persistConfigFile(state.persisted.config)
      case GeneralSettingsIntent.SetMaterialPreset(preset) =>
        updateAppearanceConfig(_.withMaterialPreset(preset)).void
      case GeneralSettingsIntent.SetPostProcessingEffect(effect) =>
        updateAppearanceConfig(_.withPostProcessingEffect(effect)).void
      case GeneralSettingsIntent.SetUiShadowsEnabled(enabled) =>
        updateAppearanceConfig(_.withUiShadowsEnabled(enabled)).void
      case GeneralSettingsIntent.SetRenderFpsTarget(target) =>
        updateAppearanceConfig(_.withRenderFpsTarget(target)).void
      case GeneralSettingsIntent.SetRenderDamageGranularity(granularity) =>
        updateAppearanceConfig(_.withRenderDamageGranularity(granularity)).void
      case GeneralSettingsIntent.SetBackgroundStyle(style) =>
        updateAppearanceConfig(_.withBackgroundStyle(style)).void
      case GeneralSettingsIntent.SetBlurRadius(r) =>
        updateAppearanceConfig(_.withBlurRadius(r)).void
      case GeneralSettingsIntent.SetAnimationDuration(ms) =>
        updateCustomMotionConfig(withEditorTextAnimationDuration(_, ms)).void
      case GeneralSettingsIntent.SetAnimationSteps(n) =>
        updateCustomMotionConfig(withEditorTextAnimationSteps(_, n)).void

  private def withEditorTextAnimationDuration(config: AppConfig, ms: Int): AppConfig =
    val newAnim =
      if ms <= 0 then None
      else
        Some(
          config.editorConfig.characterAnimation.fold(
            AnimationConfig(steps = 12, totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L))
          )(existing => existing.copy(totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L)))
        )
    config.withEditorTextAnimation(newAnim)

  private def withEditorTextAnimationSteps(config: AppConfig, n: Int): AppConfig =
    val newAnim =
      if n <= 0 then None
      else
        Some(
          config.editorConfig.characterAnimation.fold(
            AnimationConfig(steps = n, totalDuration = scala.concurrent.duration.Duration.fromNanos(200_000_000L))
          )(existing => existing.copy(steps = n))
        )
    config.withEditorTextAnimation(newAnim)

  private[manager] def persistConfigFile(config: AppConfig): IO[Unit] =
    configPersistencePath match
      case Some(path) =>
        com.serenity.config.ConfigManager.saveConfigIO(config, path).flatMap {
          case Right(_) => IO.unit
          case Left(error) =>
            logger.warn(error.cause.getOrElse(new RuntimeException(error.message)))(s"[CONFIG] ${error.message}")
        }
      case None =>
        IO.unit
