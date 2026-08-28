package com.serenity.app

import com.serenity.config.{AppConfig, MotionAccessibility, RenderFpsTarget}

/** Bundles the low-power knobs (30fps render cap, reduced motion, steady cursor) that a battery-conscious session would
  * otherwise require editing several settings for and remembering to revert -- see issue #1173. Every knob `overlay`
  * sets already exists individually; this only adds the single switch.
  */
object EcoMode:

  val EnvVar: String = "SERENITY_ECO"

  /** Overlays the eco profile onto an already-loaded config, touching only the render fps target and the motion
    * hierarchy's accessibility field -- every other setting (theme, keybindings, font, window chrome, ...) passes
    * through unchanged. `MotionAccessibility.Reduced` disables every motion family when resolved (see
    * `MotionConfig.effective`), cursor included, so this alone delivers the "steady cursor, no blink/breathe ticks"
    * half of the profile without a separate cursor-specific knob, while preserving the user's own motion baseline and
    * per-family values underneath the override.
    */
  def overlay(config: AppConfig): AppConfig =
    config
      .withRenderFpsTarget(RenderFpsTarget.Fps30)
      .withMotionAccessibility(MotionAccessibility.Reduced)

  /** Whether eco mode should activate for this launch. The `--eco` CLI flag takes priority over the environment
    * variable: it's the more deliberate, per-invocation signal, whereas `SERENITY_ECO=1` exists so an external
    * power-management script can flip every launched instance without editing config or passing a flag per invocation
    * (see issue #1173). In practice both are pure "turn it on" signals with no way to force eco off, so the priority
    * only matters as documentation of intent, not as a behavioural difference today.
    */
  def isRequested(launchOptions: LaunchOptions, env: Map[String, String] = sys.env): Boolean =
    launchOptions.eco || env.get(EnvVar).contains("1")

  /** Applies the eco overlay when requested by the CLI flag or the environment, otherwise returns `config` unchanged.
    */
  def applyIfRequested(
    config: AppConfig,
    launchOptions: LaunchOptions,
    env: Map[String, String] = sys.env
  ): AppConfig =
    if isRequested(launchOptions, env) then overlay(config) else config
