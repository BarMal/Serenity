package com.serenity.app

import com.serenity.config.AppConfig

/** Bundles the current crop of experimental prototype flags -- config knobs that ship off by default with no CLI flag,
  * text-config-file key, or in-app Settings toggle, because the feature behind them isn't finished enough to expose
  * broadly -- behind a single `--alpha` switch. Today that's just the command-runner cursor-peek prototype (#1234); a
  * future second prototype in the same state joins the same `overlay` rather than getting its own new flag.
  *
  * Unlike `EcoMode`, this has no environment-variable trigger: eco's `SERENITY_ECO` exists so an external
  * power-management script can flip every launched instance without a per-invocation flag (see issue #1173). Alpha has
  * no equivalent audience -- it's a manual, deliberate "let me go try the prototype" choice for a single invocation,
  * not something a script would want to flip system-wide, so `--alpha` is the only trigger.
  */
object AlphaMode:

  /** Overlays the alpha profile onto an already-loaded config, turning on every currently-gated experimental prototype
    * flag. Every other setting passes through unchanged.
    */
  def overlay(config: AppConfig): AppConfig =
    config.withCommandRunnerCursorPeekEnabled(true)

  /** Whether alpha mode should activate for this launch. */
  def isRequested(launchOptions: LaunchOptions): Boolean =
    launchOptions.alpha

  /** Applies the alpha overlay when requested by the CLI flag, otherwise returns `config` unchanged. */
  def applyIfRequested(config: AppConfig, launchOptions: LaunchOptions): AppConfig =
    if isRequested(launchOptions) then overlay(config) else config
