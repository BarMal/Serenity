package com.serenity.config

import com.serenity.animation.*

/** Motion, animation and transition operations on [[AppConfig]].
  *
  * Every one of these reads or writes the motion hierarchy -- the preset baseline, the per-family speeds and transition
  * kinds, and the legacy fields that mirror them -- which is one concern of its own, distinct from the plain
  * field-setting builders on [[AppConfig]] itself.
  *
  * These are extension methods in their own object rather than members of [[AppConfig]], so a call site reaches them by
  * importing this object: `import com.serenity.config.AppConfigMotionOps.*`. Scala 3 resolves an unqualified extension
  * method from lexical scope or from the receiver type's implicit scope, and this object is neither -- the import is
  * the whole mechanism, and it also records which files depend on motion.
  */
object AppConfigMotionOps:

  extension (appConfig: AppConfig)

    /** Create a new config with character animation enabled */
    def withCharacterAnimation(config: AnimationConfig): AppConfig =
      val updated = appConfig
        .withEditorConfig(appConfig.editorConfig.copy(characterAnimation = Some(config)))
        .withSurfaceConfig(appConfig.surfaceConfig.copy(motionPreset = MotionPreset.Custom))
      updated.updateAuthoritativeMotion(identity) { configuration =>
        updateMotionFamily(configuration, MotionFamily.EditorText)(_.copy(animation = Some(config)))
      }

    /** Create a new config with character animation disabled */
    def withoutCharacterAnimation: AppConfig =
      val updated = appConfig
        .withEditorConfig(appConfig.editorConfig.copy(characterAnimation = None))
        .withSurfaceConfig(
          appConfig.surfaceConfig.copy(
            motionPreset = MotionPreset.Reduced,
            editorTextTransitionSpeedScale = None,
            commandRunnerTransitionSpeedScale = None,
            uiTransitionSpeedScale = None,
            cursorTransitionSpeedScale = None,
            commandRunnerAnimation = None,
            uiAnimation = None,
            commandRunnerTransitionKind = None
          )
        )
      updated.updateAuthoritativeMotion(identity) { configuration =>
        updateMotionFamily(
          configuration.copy(baseline = MotionPreset.Reduced),
          MotionFamily.EditorText
        )(_.disabled)
      }

    def withMotionPreset(preset: MotionPreset): AppConfig =
      preset match
        case MotionPreset.Custom =>
          appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(motionPreset = MotionPreset.Custom))
        case _ =>
          val accessibility = appConfig.surfaceConfig.motionConfiguration
            .map(_.accessibility)
            .getOrElse(MotionAccessibility.Standard)
          // A preset sets the baseline; a per-family speed the user set explicitly is not part of that baseline and
          // survives it. The legacy `Option` fields are exactly the record of which ones were set explicitly, so they
          // are re-applied over the preset's own hierarchy -- otherwise choosing a preset silently discarded them from
          // the hierarchy the renderer and the saved file read, while the legacy fields kept claiming they were in
          // force.
          val presetConfiguration =
            appConfig.withExplicitFamilySpeeds(MotionConfig.forPreset(preset).copy(accessibility = accessibility))
          appConfig
            .withEditorConfig(appConfig.editorConfig.copy(characterAnimation = preset.animationConfig))
            .withSurfaceConfig(
              appConfig.surfaceConfig.copy(
                motionPreset = preset,
                motionConfiguration = Some(presetConfiguration),
                commandRunnerAnimation = preset.animationConfig,
                uiAnimation = preset.animationConfig
              )
            )

    private def withExplicitFamilySpeeds(configuration: MotionConfig): MotionConfig =
      val overrides = List(
        MotionFamily.EditorText      -> appConfig.surfaceConfig.editorTextTransitionSpeedScale,
        MotionFamily.CommandSurfaces -> appConfig.surfaceConfig.commandRunnerTransitionSpeedScale,
        MotionFamily.UiTransitions   -> appConfig.surfaceConfig.uiTransitionSpeedScale,
        MotionFamily.Cursor          -> appConfig.surfaceConfig.cursorTransitionSpeedScale
      )
      overrides.foldLeft(configuration) {
        case (current, (family, Some(scale))) =>
          current.copy(families = current.families.updated(family, current.families(family).copy(speedScale = scale)))
        case (current, _) => current
      }

    /** Marks the current resolved family values as a custom motion baseline. */
    def withCustomMotionBaseline: AppConfig =
      val fallback = MotionConfig.fromLegacy(appConfig.surfaceConfig)
      val current = appConfig.surfaceConfig.motionConfiguration
        .getOrElse(fallback)
        .withFallback(fallback)
      val editorText = appConfig.surfaceConfig.motionConfiguration
        .flatMap(_.families.get(MotionFamily.EditorText))
        .getOrElse(
          current.families(MotionFamily.EditorText).copy(animation = appConfig.editorConfig.characterAnimation)
        )
      appConfig.withSurfaceConfig(
        appConfig.surfaceConfig.copy(
          motionPreset = MotionPreset.Custom,
          motionConfiguration = Some(
            current
              .copy(
                baseline = MotionPreset.Custom,
                families = current.families.updated(MotionFamily.EditorText, editorText)
              )
              .normalized
          )
        )
      )

    /** Transition policy derived from the selected motion preset and UI speed scale. */
    def elementTransitionSettings: ElementTransitionSettings =
      appConfig.surfaceConfig.elementTransitionSettings

    /** Transition policy derived from the selected motion preset and editor text speed scale. */
    def editorInsertionTransitionSettings: ElementTransitionSettings =
      appConfig.surfaceConfig.editorInsertionTransitionSettings

    /** Transition policy for pinned panel creation. */
    def pinnedPanelTransitionSettings: ElementTransitionSettings =
      appConfig.surfaceConfig.pinnedPanelTransitionSettings

    def withElementTransitionSpeedScale(scale: Double): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(elementTransitionSpeedScale = scale)) { configuration =>
        configuration.copy(families = configuration.families.view.mapValues(_.copy(speedScale = scale)).toMap)
      }

    def withEditorTextTransitionSpeedScale(scale: Option[Double]): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(editorTextTransitionSpeedScale = scale)) { configuration =>
        updateMotionFamily(configuration, MotionFamily.EditorText)(
          _.copy(speedScale = scale.getOrElse(appConfig.surfaceConfig.elementTransitionSpeedScale))
        )
      }

    def withCommandRunnerTransitionSpeedScale(scale: Option[Double]): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(commandRunnerTransitionSpeedScale = scale)) { configuration =>
        updateMotionFamily(configuration, MotionFamily.CommandSurfaces)(
          _.copy(speedScale = scale.getOrElse(appConfig.surfaceConfig.elementTransitionSpeedScale))
        )
      }

    def withUiTransitionSpeedScale(scale: Option[Double]): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(uiTransitionSpeedScale = scale)) { configuration =>
        val speed = scale.getOrElse(appConfig.surfaceConfig.elementTransitionSpeedScale)
        updateMotionFamily(configuration, MotionFamily.UiTransitions)(_.copy(speedScale = speed))
      }

    def withCursorTransitionSpeedScale(scale: Option[Double]): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(cursorTransitionSpeedScale = scale)) { configuration =>
        updateMotionFamily(configuration, MotionFamily.Cursor)(
          _.copy(speedScale = scale.getOrElse(appConfig.surfaceConfig.elementTransitionSpeedScale))
        )
      }

    def withMotionConfiguration(configuration: MotionConfig): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(motionConfiguration = Some(configuration.normalized)))

    def withMotionAccessibility(accessibility: MotionAccessibility): AppConfig =
      val current =
        appConfig.surfaceConfig.motionConfiguration.getOrElse(MotionConfig.fromLegacy(appConfig.surfaceConfig))
      appConfig.withMotionConfiguration(current.copy(accessibility = accessibility))

    def withMotionFamilyConfiguration(family: MotionFamily, configuration: MotionFamilyConfig): AppConfig =
      val current =
        appConfig.surfaceConfig.motionConfiguration.getOrElse(MotionConfig.fromLegacy(appConfig.surfaceConfig))
      appConfig.withMotionConfiguration(current.copy(families = current.families.updated(family, configuration)))

    /** Updates editor text timing in both the legacy field and the authoritative motion family. */
    def withEditorTextAnimation(animation: Option[AnimationConfig]): AppConfig =
      val updated  = appConfig.withEditorConfig(appConfig.editorConfig.copy(characterAnimation = animation))
      val fallback = MotionConfig.fromLegacy(updated.surfaceConfig)
      val configuration = updated.surfaceConfig.motionConfiguration
        .getOrElse(fallback)
        .withFallback(fallback)
      updated.withMotionConfiguration(
        updateMotionFamily(configuration, MotionFamily.EditorText)(_.copy(animation = animation))
      )

    def withCommandRunnerAnimation(animation: Option[AnimationConfig]): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(commandRunnerAnimation = animation)) { configuration =>
        updateMotionFamily(configuration, MotionFamily.CommandSurfaces)(_.copy(animation = animation))
      }

    def withUiAnimation(animation: Option[AnimationConfig]): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(uiAnimation = animation)) { configuration =>
        updateMotionFamily(configuration, MotionFamily.UiTransitions)(_.copy(animation = animation))
      }

    def withCommandRunnerVisibleRows(rows: Option[Int]): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(commandRunnerVisibleRows = rows))

    def withCommandRunnerItemGapRows(rows: Double): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(commandRunnerItemGapRows = rows))

    def withCommandRunnerCursorGapRows(rows: Option[Double]): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(commandRunnerCursorGapRows = rows))

    def withRenderFpsTarget(target: RenderFpsTarget): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(renderFpsTarget = target))

    def withRenderDamageGranularity(granularity: RenderDamageGranularity): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(renderDamageGranularity = granularity))

    def effectiveEditorTextTransitionSpeedScale: Double =
      appConfig.surfaceConfig.effectiveEditorTextTransitionSpeedScale

    def effectiveCommandRunnerTransitionSpeedScale: Double =
      appConfig.surfaceConfig.effectiveCommandRunnerTransitionSpeedScale

    def effectiveUiTransitionSpeedScale: Double =
      appConfig.surfaceConfig.effectiveUiTransitionSpeedScale

    def effectiveCursorTransitionSpeedScale: Double =
      appConfig.surfaceConfig.effectiveCursorTransitionSpeedScale

    /** Character insertion animation after applying the effective editor text motion speed. */
    def scaledCharacterAnimation: Option[AnimationConfig] =
      appConfig.surfaceConfig.motionConfiguration match
        case Some(_) =>
          val motion = appConfig.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.EditorText)
          Option.when(motion.enabled)(AppConfig.scaledAnimation(motion.animation, motion.speedScale)).flatten
        case None =>
          AppConfig.scaledAnimation(
            appConfig.editorConfig.characterAnimation,
            appConfig.effectiveEditorTextTransitionSpeedScale
          )

    /** Command runner animation after applying the effective command runner motion speed. */
    def scaledCommandRunnerAnimation: Option[AnimationConfig] =
      val motion = appConfig.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.CommandSurfaces)
      Option.when(motion.enabled)(AppConfig.scaledAnimation(motion.animation, motion.speedScale)).flatten

    /** General UI animation after applying the effective UI motion speed. */
    def scaledUiAnimation: Option[AnimationConfig] =
      val motion = appConfig.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.UiTransitions)
      Option.when(motion.enabled)(AppConfig.scaledAnimation(motion.animation, motion.speedScale)).flatten

    def withEditorInsertionTransitionKind(kind: TransitionKind): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(editorInsertionTransitionKind = kind)) { configuration =>
        updateMotionFamily(configuration, MotionFamily.EditorText)(_.copy(transitionKind = kind))
      }

    def withCommandRunnerTransitionKind(kind: Option[TransitionKind]): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(commandRunnerTransitionKind = kind)) { configuration =>
        kind.fold(configuration)(transition =>
          updateMotionFamily(configuration, MotionFamily.CommandSurfaces)(_.copy(transitionKind = transition))
        )
      }

    def effectiveCommandRunnerTransitionKind: TransitionKind =
      appConfig.surfaceConfig.effectiveCommandRunnerTransitionKind

    def withPanelOpenTransitionKind(kind: Option[TransitionKind]): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(panelOpenTransitionKind = kind)) { configuration =>
        kind.fold(configuration)(transition =>
          updatePanelTransition(configuration, TransitionScope.PanelOpen, transition)
        )
      }

    def withPanelCloseTransitionKind(kind: Option[TransitionKind]): AppConfig =
      appConfig.updateAuthoritativeMotion(_.copy(panelCloseTransitionKind = kind)) { configuration =>
        kind.fold(configuration)(transition =>
          updatePanelTransition(configuration, TransitionScope.PanelClose, transition)
        )
      }

    def effectivePanelOpenTransitionKind: TransitionKind =
      appConfig.surfaceConfig.effectivePanelOpenTransitionKind

    def effectivePanelCloseTransitionKind: TransitionKind =
      appConfig.surfaceConfig.effectivePanelCloseTransitionKind

    /** One line is the least a notch can usefully move; the upper bound keeps a mis-typed value from turning a notch
      * into a jump across the document.
      */
    def withWheelScrollLines(lines: Int): AppConfig =
      appConfig.withInputConfig(appConfig.inputConfig.copy(wheelScrollLines = AppConfig.clampWheelScrollLines(lines)))

    /** Apply a motion change to both the legacy field that describes it and the authoritative hierarchy.
      *
      * The hierarchy is materialised from the legacy fields when there is none yet. Updating it only when one already
      * existed meant a change made before any hierarchy was built -- setting an element-wide speed on a fresh
      * configuration, say -- landed in the legacy field alone, and the next thing to install a hierarchy (choosing a
      * motion preset) silently dropped it: the configuration then held one value in its legacy field and another in the
      * hierarchy the renderer and the saved file both use.
      */
    private def updateAuthoritativeMotion(
      updateSurface: SurfaceConfig => SurfaceConfig
    )(
      updateConfiguration: MotionConfig => MotionConfig
    ): AppConfig =
      val updatedSurface = updateSurface(appConfig.surfaceConfig)
      val current =
        appConfig.surfaceConfig.motionConfiguration.getOrElse(MotionConfig.fromLegacy(appConfig.surfaceConfig))
      val fallback             = MotionConfig.fromLegacy(appConfig.surfaceConfig, current.baseline)
      val updatedConfiguration = updateConfiguration(current.withFallback(fallback)).normalized
      appConfig.withSurfaceConfig(updatedSurface.copy(motionConfiguration = Some(updatedConfiguration)))

  private def updateMotionFamily(
    configuration: MotionConfig,
    family: MotionFamily
  )(
    update: MotionFamilyConfig => MotionFamilyConfig
  ): MotionConfig =
    configuration.copy(families = configuration.families.updated(family, update(configuration.families(family))))

  private def updatePanelTransition(
    configuration: MotionConfig,
    scope: TransitionScope,
    transition: TransitionKind
  ): MotionConfig =
    updateMotionFamily(configuration, MotionFamily.PinnedPanels) { panel =>
      val overrides = panel.transitionOverrides.updated(scope, transition)
      // `enabled` is derived from `transitionKind` (this family's open transition), not from either override
      // independently, so a config with the open transition off and the close transition on now resolves the whole
      // family to disabled rather than the previous open-or-close union -- see the equivalent note in
      // `MotionConfig.fromLegacy`.
      panel.copy(
        transitionKind = overrides.getOrElse(TransitionScope.PanelOpen, panel.transitionKind),
        transitionOverrides = overrides
      )
    }
