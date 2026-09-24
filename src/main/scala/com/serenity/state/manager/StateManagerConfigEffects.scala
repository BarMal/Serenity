package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.animation.AnimationConfig
import com.serenity.animation.sprite.CompanionSpriteConfig
import com.serenity.command.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, LineNumberLayout, StatusLinePlacement, StatusSegment, VisualFlairLevel}
import com.serenity.session.{SessionPersistence, SessionSaveTrigger}
import com.serenity.spellcheck.{DictionaryWord, SpellChecker}
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
      applyConfigUpdate(update).flatTap(motionCancellation.cancelDisabledMotion(previousState.persisted.config, _))
    }

  private def updateMotionAccessibility(accessibility: com.serenity.config.MotionAccessibility): IO[AppConfig] =
    updateMotionConfig(_.withMotionAccessibility(accessibility))

  private[manager] def updateCompanionSpriteConfig(update: CompanionSpriteConfig => CompanionSpriteConfig): IO[Unit] =
    updateAppearanceConfig(config => config.withCompanionSpriteConfig(update(config.companionSpriteConfig)))
      .flatTap(config => stateRef.update(state => syncCompanionSpritePanel(state, config)))
      .void

  private[manager] def updateVisualFlairLevel(level: VisualFlairLevel): IO[Unit] =
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

  private val motionCancellation =
    StateManagerMotionCancellation(editor.updateModelValidated)

  private def updateCustomMotionConfig(update: AppConfig => AppConfig): IO[AppConfig] =
    updateMotionConfig(config => update(config).withCustomMotionBaseline)

  private[manager] def updateTextDisplayConfig(update: AppConfig => AppConfig): IO[AppConfig] =
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
        IO(
          com.serenity.ui.renderer.RendererFrameState
            .configureCacheCapacity(config.surfaceConfig.rendererFrameStateCacheCapacity)
        )
      )
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
      case SettingsIntent.StatusLine(statusLineIntent)   => interpretStatusLineIntent(statusLineIntent)
      case SettingsIntent.Cursor(cursorIntent)           => interpretCursorIntent(cursorIntent)
      case SettingsIntent.TextDisplay(textDisplayIntent) => interpretTextDisplayIntent(textDisplayIntent)
      case SettingsIntent.InterfaceChrome(interfaceChromeIntent) =>
        interpretInterfaceChromeIntent(interfaceChromeIntent)
      case SettingsIntent.Decoration(decorationIntent) => interpretDecorationIntent(decorationIntent)
      case SettingsIntent.SpellCheck(spellCheckIntent) => interpretSpellCheckIntent(spellCheckIntent, state)
      case SettingsIntent.General(generalIntent)       => interpretGeneralSettingsIntent(generalIntent, state)

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

  private def interpretStatusLineIntent(intent: StatusLineIntent): IO[Unit] =
    intent match
      case StatusLineIntent.SetSegmentIncluded(segment, included) =>
        updateTextDisplayConfig { config =>
          val current = config.statusLine.segments
          config.withStatusLineSegments(
            if included then StatusSegment.include(current, segment) else current.filterNot(_ == segment)
          )
        }.void
      case StatusLineIntent.MoveSegmentEarlier(segment) =>
        updateTextDisplayConfig(config =>
          config.withStatusLineSegments(StatusSegment.move(config.statusLine.segments, segment, -1))
        ).void
      case StatusLineIntent.MoveSegmentLater(segment) =>
        updateTextDisplayConfig(config =>
          config.withStatusLineSegments(StatusSegment.move(config.statusLine.segments, segment, 1))
        ).void
      case StatusLineIntent.SetPlacement(placement) =>
        updateTextDisplayConfig(_.withStatusLinePlacement(placement)).void
      case StatusLineIntent.ToggleVisibility =>
        updateTextDisplayConfig { config =>
          val next =
            if config.statusLine.placement == StatusLinePlacement.Off then StatusLinePlacement.Pinned
            else StatusLinePlacement.Off
          config.withStatusLinePlacement(next)
        }.void

  private def interpretTextDisplayIntent(intent: TextDisplayIntent): IO[Unit] =
    intent match
      case TextDisplayIntent.ToggleLineNumbers =>
        updateTextDisplayConfig(config => config.withLineNumbers(!config.surfaceConfig.showLineNumbers)).void
      case TextDisplayIntent.ToggleWordWrap =>
        updateTextDisplayConfig(config => config.withWordWrap(!config.surfaceConfig.wordWrapEnabled)).void
      case TextDisplayIntent.ToggleFocusedTextBody =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(!config.surfaceConfig.focusedTextBodyEnabled)).void
      case TextDisplayIntent.ToggleContextualToolbar =>
        editor.enqueueEvent(com.serenity.keystroke.events.ToggleContextualToolbar)
      case TextDisplayIntent.TogglePaneHeaders =>
        updateTextDisplayConfig(config => config.withPaneHeaders(!config.surfaceConfig.showPaneHeaders)).void
      case TextDisplayIntent.ToggleVisualLineCursorNavigation =>
        updateTextDisplayConfig(config =>
          config.withVisualLineCursorNavigation(!config.surfaceConfig.visualLineCursorNavigation)
        ).void
      case TextDisplayIntent.ToggleTypewriterScrolling =>
        updateTextDisplayConfig(config =>
          config.withTypewriterScrolling(!config.surfaceConfig.typewriterScrollingEnabled)
        ).void
      case TextDisplayIntent.SetLineNumbers(enabled) =>
        updateTextDisplayConfig(config => config.withLineNumbers(enabled)).void
      case TextDisplayIntent.SetLineNumberSide(side) =>
        updateLineNumberLayout(_.copy(side = side))
      case TextDisplayIntent.SetLineNumberMarginLeft(cells) =>
        updateLineNumberLayout(_.copy(marginLeft = Some(cells)))
      case TextDisplayIntent.SetLineNumberMarginRight(cells) =>
        updateLineNumberLayout(_.copy(marginRight = cells))
      case TextDisplayIntent.SetLineNumberPadding(cells) =>
        updateLineNumberLayout(_.copy(padding = Some(cells)))
      case TextDisplayIntent.SetWordWrap(enabled) =>
        updateTextDisplayConfig(config => config.withWordWrap(enabled)).void
      case TextDisplayIntent.SetVisualLineCursorNavigation(enabled) =>
        updateTextDisplayConfig(config => config.withVisualLineCursorNavigation(enabled)).void
      case TextDisplayIntent.SetTypewriterScrolling(enabled) =>
        updateTextDisplayConfig(config => config.withTypewriterScrolling(enabled)).void
      case TextDisplayIntent.ToggleColumnMode =>
        updateTextDisplayConfig(config => config.withColumnMode(!config.surfaceConfig.columnModeEnabled)).void
      case TextDisplayIntent.SetColumnMode(enabled) =>
        updateTextDisplayConfig(config => config.withColumnMode(enabled)).void
      case TextDisplayIntent.SetColumnTargetWidth(cells) =>
        updateTextDisplayConfig(config => config.withColumnTargetWidth(cells)).void
      case TextDisplayIntent.SetColumnGap(cells) =>
        updateTextDisplayConfig(config => config.withColumnGap(cells)).void
      case TextDisplayIntent.SetFocusedTextBody(enabled) =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(enabled)).void
      case TextDisplayIntent.SetContextualToolbarEnabled(enabled) =>
        updateTextDisplayConfig(config => config.withContextualToolbarEnabled(enabled)).void
      case TextDisplayIntent.SetContextualToolbarDisplayMode(mode) =>
        updateTextDisplayConfig(config => config.withContextualToolbarDisplayMode(mode)).void
      case TextDisplayIntent.SetTextAreaLeftInset(value) =>
        updateTextDisplayConfig(_.withTextAreaLeftInset(value)).void
      case TextDisplayIntent.SetTextAreaRightInset(value) =>
        updateTextDisplayConfig(_.withTextAreaRightInset(value)).void
      case TextDisplayIntent.SetTextAreaTopInset(value) =>
        updateTextDisplayConfig(_.withTextAreaTopInset(value)).void
      case TextDisplayIntent.SetTextAreaBottomInset(value) =>
        updateTextDisplayConfig(_.withTextAreaBottomInset(value)).void

  private def updateLineNumberLayout(update: LineNumberLayout => LineNumberLayout): IO[Unit] =
    updateTextDisplayConfig(config => config.withLineNumberLayout(update(config.lineNumberLayout))).void

  private def interpretInterfaceChromeIntent(intent: InterfaceChromeIntent): IO[Unit] =
    intent match
      case InterfaceChromeIntent.SetCommandRunnerShowKeyHints(enabled) =>
        updateAppearanceConfig(_.withCommandRunnerShowKeyHints(enabled)).void
      case InterfaceChromeIntent.SetUiElementGap(gap) =>
        updateAppearanceConfig(_.withUiElementGap(Some(gap))).void
      case InterfaceChromeIntent.SetUiCornerRadiusPx(radius) =>
        updateAppearanceConfig(_.withUiCornerRadiusPx(radius)).void
      case InterfaceChromeIntent.SetUiOutlineThicknessPx(thickness) =>
        updateAppearanceConfig(_.withUiOutlineThicknessPx(thickness)).void
      case InterfaceChromeIntent.SetInterfaceDensity(density) =>
        updateAppearanceConfig(_.withInterfaceDensity(density)).void
      case InterfaceChromeIntent.SetWindowChromeMode(mode) =>
        updateAppearanceConfig(_.withWindowChromeMode(mode)).void
      case InterfaceChromeIntent.SetWheelScrollLines(lines) =>
        updateConfig(_.withWheelScrollLines(lines)).void

  private def interpretDecorationIntent(intent: DecorationIntent): IO[Unit] =
    intent match
      case DecorationIntent.SetCompanionSpriteEnabled(enabled) =>
        updateCompanionSpriteConfig(_.copy(enabled = enabled))
      case DecorationIntent.SetCompanionSpriteTypingCycle(cycle) =>
        updateCompanionSpriteConfig(_.copy(typingCycle = cycle))
      case DecorationIntent.SetCompanionSpriteTypingActiveTicks(ticks) =>
        updateCompanionSpriteConfig(_.copy(typingActiveTicks = ticks))
      case DecorationIntent.SetCompanionSpriteTypingFastActiveTicks(ticks) =>
        updateCompanionSpriteConfig(_.copy(typingFastActiveTicks = ticks))
      case DecorationIntent.SetCompanionSpriteTypingFastThresholdMs(ms) =>
        updateCompanionSpriteConfig(_.copy(typingFastThresholdMs = ms))
      case DecorationIntent.SetVisualFlairLevel(level) =>
        updateVisualFlairLevel(level)

  private def interpretSpellCheckIntent(intent: SpellCheckIntent, state: AppState): IO[Unit] =
    intent match
      case SpellCheckIntent.SetSpellCheckEnabled(enabled) =>
        updateSpellCheckConfig(_.copy(enabled = enabled))
      case SpellCheckIntent.SetSpellCheckLanguages(languages) =>
        updateSpellCheckConfig(_.copy(languages = languages))
      case SpellCheckIntent.SetSpellCheckDictionaryPaths(paths) =>
        updateSpellCheckConfig(_.copy(dictionaryPaths = paths))
      case SpellCheckIntent.SetSpellCheckWords(words) =>
        updateSpellCheckConfig(_.copy(additionalWords = words))
      case SpellCheckIntent.AddWordAtCursorToDictionary =>
        addFlaggedWordAtCursorToDictionary(state)

  // #1531: silently a no-op when the cursor is not on a flagged word -- mirrors how `delete-document-comment`
  // is a no-op with no comment at the cursor, rather than surfacing an error for a command reachable from a
  // static context-menu/command-palette entry that doesn't know in advance whether it applies.
  private def addFlaggedWordAtCursorToDictionary(state: AppState): IO[Unit] =
    SpellChecker.flaggedWordAtCursor(state) match
      case Some(word) =>
        val normalized = DictionaryWord.normalize(word)
        updateSpellCheckConfig(config => config.copy(additionalWords = (config.additionalWords :+ normalized).distinct))
      case None =>
        IO.unit

  private def interpretGeneralSettingsIntent(intent: GeneralSettingsIntent, state: AppState): IO[Unit] =
    intent match
      case GeneralSettingsIntent.OpenSettings =>
        stateRef.get.flatMap { current =>
          val newState = CommandRunnerReducer.openSettings(current, CommandRegistry.withToggleUI)(using balance)
          editor.validateAndUpdateState(newState, current)
        }
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
