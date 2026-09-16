package com.serenity.config

/** Which config-file keys name a `motion.*` setting: the current spelling of each one, the older spellings still read
  * (the whole `ui.motion.*` namespace among them), and the per-setting key sets [[SurfaceConfigSchemaParser]]
  * dispatches on.
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

  val motionFamilyPrefix: String       = ConfigGroups.motionFamilyPrefix
  val legacyMotionFamilyPrefix: String = ConfigGroups.legacyMotionFamilyPrefix

  private val familyFields: Set[String] =
    Set(
      "enabled",
      "transition",
      "animation",
      "animation.preset",
      "animation.duration_ms",
      "animation.steps",
      "speed_scale"
    )

  private def familyKeysUnder(prefix: String): Set[String] =
    MotionFamily.values.flatMap(family => familyFields.map(field => s"$prefix${family.configKey}.$field")).toSet ++
      Set(s"${prefix}pinned_panels.open_transition", s"${prefix}pinned_panels.close_transition")

  /** Each setting's spellings: the current one first, then the older ones still read. */
  private val spellings: List[(String, List[String])] = List(
    "motion.preset"        -> List("ui.motion.preset", "ui.motion", "ui_motion", "motion_preset"),
    "motion.accessibility" -> List("ui.motion.accessibility"),
    "motion.speed_scale"   -> List("ui.motion.speed_scale", "ui_motion_speed_scale", "motion_speed_scale"),
    "motion.editor_text.speed_scale" ->
      List(
        "ui.motion.editor_text.speed_scale",
        "ui.motion.editor.text.speed_scale",
        "ui_motion_editor_text_speed_scale"
      ),
    "motion.command_runner.speed_scale" ->
      List(
        "ui.motion.command_runner.speed_scale",
        "ui.motion.command.runner.speed_scale",
        "ui_motion_command_runner_speed_scale"
      ),
    "motion.ui.speed_scale" ->
      List(
        "ui.motion.ui.speed_scale",
        "ui.motion.ui_elements.speed_scale",
        "ui.motion.ui.elements.speed_scale",
        "ui_motion_ui_speed_scale"
      ),
    "motion.cursor.speed_scale" ->
      List(
        "ui.motion.cursor.speed_scale",
        "ui.motion.cursor_speed_scale",
        "ui.motion.cursor.speed.scale",
        "ui_motion_cursor_speed_scale"
      ),
    "motion.command_runner" -> List("ui.motion.command_runner", "ui.motion.command.runner", "ui_motion_command_runner"),
    "motion.command_runner_reveal" ->
      List("ui.motion.command_runner_reveal", "ui.motion.command.runner.reveal", "ui_motion_command_runner_reveal"),
    "motion.ui"          -> List("ui.motion.ui", "ui.motion.ui_elements", "ui.motion.ui.elements", "ui_motion_ui"),
    "motion.editor_text" -> List("ui.motion.editor_text", "ui.motion.editor.text", "ui_motion_editor_text"),
    "motion.panel_open"  -> List("ui.motion.panel_open", "ui.motion.panel.open", "ui_motion_panel_open"),
    "motion.panel_close" -> List("ui.motion.panel_close", "ui.motion.panel.close", "ui_motion_panel_close")
  )

  private def keysFor(current: String): Set[String] =
    spellings.collectFirst { case (`current`, older) => (current :: older).toSet }.getOrElse(Set(current))

  val currentKeys: Set[String] = spellings.map(_._1).toSet ++ familyKeysUnder(motionFamilyPrefix)

  val deprecatedKeys: Map[String, String] =
    spellings.flatMap { case (current, older) => older.map(_ -> current) }.toMap ++
      familyKeysUnder(legacyMotionFamilyPrefix).map(key =>
        key -> (motionFamilyPrefix + key.stripPrefix(legacyMotionFamilyPrefix))
      )

  val motionPresetKeys: Set[String]        = keysFor("motion.preset")
  val motionAccessibilityKeys: Set[String] = keysFor("motion.accessibility")
  val motionFamilyKeys: Set[String] = familyKeysUnder(motionFamilyPrefix) ++ familyKeysUnder(legacyMotionFamilyPrefix)
  val elementTransitionSpeedScaleKeys: Set[String]       = keysFor("motion.speed_scale")
  val editorTextTransitionSpeedScaleKeys: Set[String]    = keysFor("motion.editor_text.speed_scale")
  val commandRunnerTransitionSpeedScaleKeys: Set[String] = keysFor("motion.command_runner.speed_scale")
  val uiTransitionSpeedScaleKeys: Set[String]            = keysFor("motion.ui.speed_scale")
  val cursorTransitionSpeedScaleKeys: Set[String]        = keysFor("motion.cursor.speed_scale")
  val commandRunnerAnimationKeys: Set[String]            = keysFor("motion.command_runner")
  val commandRunnerTransitionKeys: Set[String]           = keysFor("motion.command_runner_reveal")
  val uiAnimationKeys: Set[String]                       = keysFor("motion.ui")
  val editorTextTransitionKeys: Set[String]              = keysFor("motion.editor_text")
  val panelOpenTransitionKeys: Set[String]               = keysFor("motion.panel_open")
  val panelCloseTransitionKeys: Set[String]              = keysFor("motion.panel_close")

  private val handledKeys: Set[String] =
    spellings.flatMap { case (current, older) => current :: older }.toSet ++ motionFamilyKeys

  def handles(key: String): Boolean =
    handledKeys.contains(key)
