package com.serenity.app

import com.serenity.config.AppConfigOps.*
import com.serenity.config.{AppConfig, CursorMode, RenderFpsTarget}

/** Bundles the low-power knobs a battery-conscious session would otherwise require editing several settings for and
  * remembering to revert -- see issues #1173 and #1886: the 30fps render cap, a steady caret that wakes nothing, no
  * chapter ghosts, and longer pauses before the spell-check analysis and the outline re-parse run. The reduced-motion
  * half of the profile went away with the motion settings.
  */
object EcoMode:

  val EnvVar: String = "SERENITY_ECO"

  val AnalysisDebounceMillis: Long = 500L
  val OutlineDebounceMillis: Long  = 500L

  /** Overlays the eco profile onto an already-loaded config, touching only the settings above -- every other setting
    * (theme, keybindings, font, window chrome, ...) passes through unchanged. A debounce the user already set longer
    * than eco's is kept.
    */
  def overlay(config: AppConfig): AppConfig =
    config
      .withRenderFpsTarget(RenderFpsTarget.Fps30)
      .withCursorMode(CursorMode.Steady)
      .withChapterGhosts(false)
      .withAnalysisDebounceMillis(config.documentConfig.analysisDebounceMillis.max(AnalysisDebounceMillis))
      .withOutlineDebounceMillis(config.documentConfig.outlineDebounceMillis.max(OutlineDebounceMillis))

  /** Whether eco mode should activate for this launch. The `--eco` CLI flag takes priority over the environment
    * variable: it's the more deliberate, per-invocation signal, whereas `SERENITY_ECO=1` exists so an external
    * power-management script can flip every launched instance without editing config or passing a flag per invocation
    * (see issue #1173). In practice both are pure "turn it on" signals with no way to force eco off, so the priority
    * only matters as documentation of intent, not as a behavioural difference today.
    */
  def isRequested(launchOptions: LaunchOptions, env: Map[String, String] = sys.env): Boolean =
    launchOptions.eco || env.get(EnvVar).contains("1")

  def applyIfRequested(
    config: AppConfig,
    launchOptions: LaunchOptions,
    env: Map[String, String] = sys.env
  ): AppConfig =
    if isRequested(launchOptions, env) then overlay(config) else config
