package com.serenity.config

/** Command-runner spacing, render cadence and wheel-scroll operations on [[AppConfig]].
  *
  * These are extension methods in their own object rather than members of [[AppConfig]], so a call site reaches them by
  * importing this object: `import com.serenity.config.AppConfigOps.*`.
  */
object AppConfigOps:

  extension (appConfig: AppConfig)

    def withCommandRunnerVisibleRows(rows: Option[Int]): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(commandRunnerVisibleRows = rows))

    /** issue #1046 (review follow-up): the one place `commandRunnerVisibleRows` resolves against the current interface
      * density -- mirrors [[effectiveCommandRunnerItemGapRows]] below. An explicit override (from an old
      * `command_runner.visible_rows` config value) wins; otherwise it falls back to that density's own
      * [[InterfaceDensityMetrics.visibleRows]], the same "one density control" every other command palette
      * row-spacing/height knob already falls back to.
      */
    def effectiveCommandRunnerVisibleRows: Int =
      appConfig.surfaceConfig.commandRunnerVisibleRows.getOrElse(
        InterfaceDensityMetrics.forDensity(appConfig.interfaceDensity).visibleRows
      )

    def withCommandRunnerItemGapRows(rows: Option[Double]): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(commandRunnerItemGapRows = rows))

    /** issue #1046: the one place `commandRunnerItemGapRows` resolves against the current interface density -- an
      * explicit override (from an old `command_runner.item_gap_rows` config value) wins; otherwise it falls back to
      * that density's own [[InterfaceDensityMetrics.itemGapRows]], the same "one density control" every other command
      * palette row-spacing/height knob already falls back to.
      */
    def effectiveCommandRunnerItemGapRows: Double =
      appConfig.surfaceConfig.commandRunnerItemGapRows.getOrElse(
        InterfaceDensityMetrics.forDensity(appConfig.interfaceDensity).itemGapRows
      )

    def withCommandRunnerCursorGapRows(rows: Option[Double]): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(commandRunnerCursorGapRows = rows))

    /** issue #1046 (review follow-up): the one place `commandRunnerCursorGapRows` resolves -- mirrors
      * [[effectiveCommandRunnerItemGapRows]]/[[effectiveCommandRunnerVisibleRows]] above. An explicit override wins;
      * otherwise this falls back to the same chain [[com.serenity.ui.layout.FloatingSurfaceLayout]] already used pre-PR
      * for the command palette's cursor gap -- a general `uiElementGap` override (`ui.element_gap`) takes priority over
      * the density-derived [[InterfaceDensityMetrics.overlayGapRows]] default -- so this is not a behavior change, only
      * a named accessor for logic that used to live only in that layout code.
      */
    def effectiveCommandRunnerCursorGapRows: Double =
      appConfig.surfaceConfig.commandRunnerCursorGapRows.getOrElse(
        appConfig.uiElementGap
          .filter(_ > 0.0)
          .getOrElse(InterfaceDensityMetrics.forDensity(appConfig.interfaceDensity).overlayGapRows.toDouble)
      )

    def withRenderFpsTarget(target: RenderFpsTarget): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(renderFpsTarget = target))

    def withRenderDamageGranularity(granularity: RenderDamageGranularity): AppConfig =
      appConfig.withSurfaceConfig(appConfig.surfaceConfig.copy(renderDamageGranularity = granularity))

    /** One line is the least a notch can usefully move; the upper bound keeps a mis-typed value from turning a notch
      * into a jump across the document.
      */
    def withWheelScrollLines(lines: Int): AppConfig =
      appConfig.withInputConfig(appConfig.inputConfig.copy(wheelScrollLines = AppConfig.clampWheelScrollLines(lines)))
