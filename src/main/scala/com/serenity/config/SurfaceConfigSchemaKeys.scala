package com.serenity.config

/** Which config-file keys name a `ui.motion.*` setting: the current spelling of each one, the older spellings still
  * read, and the per-setting key sets [[SurfaceConfigSchemaParser]] dispatches on.
  *
  * The material/post-processing/display/command-runner/text-area/viewport settings this used to also cover are gone
  * (#1406) -- [[ConfigRegistry]] already owned parsing, validation and writing for every one of them end-to-end, so
  * [[ConfigManager.parseConfig]] never actually reached this module's copy (`ConfigRegistry.find` is tried first). What
  * is left is genuinely load-bearing: the motion hierarchy has no [[ConfigField]] of its own to read it back with (see
  * [[ConfigGroups]], which only writes it), so this is still the one place that parses it.
  *
  * Vocabulary only -- nothing here parses a value. [[handles]] is what a caller asks before handing a key to
  * [[SurfaceConfigSchemaParser.parse]].
  */
object SurfaceConfigSchemaKeys:

  val currentKeys: Set[String] = Set(
    "ui.motion.preset",
    "ui.motion",
    "motion.preset",
    "ui.motion.speed_scale",
    "motion.speed_scale",
    "ui.motion.editor_text.speed_scale",
    "ui.motion.editor.text.speed_scale",
    "ui.motion.command_runner.speed_scale",
    "ui.motion.command.runner.speed_scale",
    "ui.motion.ui.speed_scale",
    "ui.motion.ui_elements.speed_scale",
    "ui.motion.ui.elements.speed_scale",
    "ui.motion.cursor.speed_scale",
    "ui.motion.cursor_speed_scale",
    "ui.motion.cursor.speed.scale",
    "ui.motion.command_runner",
    "ui.motion.command.runner",
    "ui.motion.command_runner_reveal",
    "ui.motion.command.runner.reveal",
    "ui.motion.ui",
    "ui.motion.ui_elements",
    "ui.motion.ui.elements",
    "ui.motion.editor_text",
    "ui.motion.editor.text",
    "ui.motion.panel_open",
    "ui.motion.panel.open",
    "ui.motion.panel_close",
    "ui.motion.panel.close"
  ) ++ Set("ui.motion.accessibility") ++ MotionFamily.values.flatMap { family =>
    Set(
      "enabled",
      "transition",
      "animation",
      "animation.preset",
      "animation.duration_ms",
      "animation.steps",
      "speed_scale"
    ).map(field => s"ui.motion.family.${family.configKey}.$field")
  } ++ Set(
    "ui.motion.family.pinned_panels.open_transition",
    "ui.motion.family.pinned_panels.close_transition"
  )

  val deprecatedKeys: Map[String, String] = Map(
    "ui_motion"                            -> "ui.motion",
    "motion_preset"                        -> "motion.preset",
    "ui_motion_speed_scale"                -> "ui.motion.speed_scale",
    "motion_speed_scale"                   -> "motion.speed_scale",
    "ui_motion_editor_text_speed_scale"    -> "ui.motion.editor_text.speed_scale",
    "ui_motion_command_runner_speed_scale" -> "ui.motion.command_runner.speed_scale",
    "ui_motion_ui_speed_scale"             -> "ui.motion.ui.speed_scale",
    "ui_motion_cursor_speed_scale"         -> "ui.motion.cursor.speed_scale",
    "ui_motion_command_runner"             -> "ui.motion.command_runner",
    "ui_motion_command_runner_reveal"      -> "ui.motion.command_runner_reveal",
    "ui_motion_ui"                         -> "ui.motion.ui",
    "ui_motion_editor_text"                -> "ui.motion.editor_text",
    "ui_motion_panel_open"                 -> "ui.motion.panel_open",
    "ui_motion_panel_close"                -> "ui.motion.panel_close"
  )

  /** `ui.motion.preset` is the spelling written. `ui.motion` is a leaf on a path whose children (`ui.motion.family`,
    * `ui.motion.accessibility`) HOCON would resolve by dropping it, so it survived only by being written as a quoted
    * key; it stays readable for files that already have it.
    */
  val motionPresetKeys: Set[String] =
    Set("ui.motion.preset", "ui.motion", "ui_motion", "motion.preset", "motion_preset")
  val motionAccessibilityKeys: Set[String] = Set("ui.motion.accessibility")
  val motionFamilyPrefix                   = "ui.motion.family."

  val motionFamilyKeys: Set[String] = MotionFamily.values.flatMap { family =>
    Set(
      "enabled",
      "transition",
      "animation",
      "animation.preset",
      "animation.duration_ms",
      "animation.steps",
      "speed_scale"
    ).map(field => s"$motionFamilyPrefix${family.configKey}.$field")
  }.toSet ++ Set(
    s"${motionFamilyPrefix}pinned_panels.open_transition",
    s"${motionFamilyPrefix}pinned_panels.close_transition"
  )

  val elementTransitionSpeedScaleKeys: Set[String] =
    Set("ui.motion.speed_scale", "motion.speed_scale", "ui_motion_speed_scale", "motion_speed_scale")

  val editorTextTransitionSpeedScaleKeys: Set[String] =
    Set("ui.motion.editor_text.speed_scale", "ui.motion.editor.text.speed_scale", "ui_motion_editor_text_speed_scale")

  val commandRunnerTransitionSpeedScaleKeys: Set[String] =
    Set(
      "ui.motion.command_runner.speed_scale",
      "ui.motion.command.runner.speed_scale",
      "ui_motion_command_runner_speed_scale"
    )

  val uiTransitionSpeedScaleKeys: Set[String] =
    Set(
      "ui.motion.ui.speed_scale",
      "ui.motion.ui_elements.speed_scale",
      "ui.motion.ui.elements.speed_scale",
      "ui_motion_ui_speed_scale"
    )

  val cursorTransitionSpeedScaleKeys: Set[String] =
    Set(
      "ui.motion.cursor.speed_scale",
      "ui.motion.cursor_speed_scale",
      "ui.motion.cursor.speed.scale",
      "ui_motion_cursor_speed_scale"
    )

  val commandRunnerAnimationKeys: Set[String] =
    Set("ui.motion.command_runner", "ui.motion.command.runner", "ui_motion_command_runner")

  val commandRunnerTransitionKeys: Set[String] =
    Set("ui.motion.command_runner_reveal", "ui.motion.command.runner.reveal", "ui_motion_command_runner_reveal")

  val uiAnimationKeys: Set[String] =
    Set("ui.motion.ui", "ui.motion.ui_elements", "ui.motion.ui.elements", "ui_motion_ui")

  val editorTextTransitionKeys: Set[String] =
    Set("ui.motion.editor_text", "ui.motion.editor.text", "ui_motion_editor_text")

  val panelOpenTransitionKeys: Set[String] =
    Set("ui.motion.panel_open", "ui.motion.panel.open", "ui_motion_panel_open")

  val panelCloseTransitionKeys: Set[String] =
    Set("ui.motion.panel_close", "ui.motion.panel.close", "ui_motion_panel_close")

  private val handledKeys: Set[String] =
    motionPresetKeys ++
      motionAccessibilityKeys ++
      motionFamilyKeys ++
      elementTransitionSpeedScaleKeys ++
      editorTextTransitionSpeedScaleKeys ++
      commandRunnerTransitionSpeedScaleKeys ++
      uiTransitionSpeedScaleKeys ++
      cursorTransitionSpeedScaleKeys ++
      commandRunnerAnimationKeys ++
      commandRunnerTransitionKeys ++
      uiAnimationKeys ++
      editorTextTransitionKeys ++
      panelOpenTransitionKeys ++
      panelCloseTransitionKeys

  def handles(key: String): Boolean =
    handledKeys.contains(key)
