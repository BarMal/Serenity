package com.serenity.config

/** The keys [[SurfaceConfig.Schema]] recognises: the current spelling of every surface setting, the older spellings
  * still read, and every setting's full set of accepted keys. Split out of [[SurfaceConfig]] to keep that file under
  * the architecture ratchet's file-length target -- [[SurfaceConfig.Schema]] re-exports these members, so callers see
  * no difference.
  */
object SurfaceConfigSchemaKeys:

  val currentKeys: Set[String] = Set(
    "ui.material",
    "material.preset",
    "ui.post_processing",
    "ui.shadows",
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
    "ui.motion.panel.close",
    "command_runner.visible_rows",
    "command.runner.visible.rows",
    "command_runner.item_gap_rows",
    "command.runner.item.gap.rows",
    "command_runner.cursor_gap_rows",
    "command.runner.cursor.gap.rows",
    "render.fps",
    "ui.render.fps",
    "render.damage_granularity",
    "render.damage.granularity",
    "display.cursor_info_bar_background_alpha",
    "display.cursor_info_bar.background_alpha",
    "display.word_wrap",
    "display.word.wrap",
    "display.visual_line_navigation",
    "display.visual.line.navigation",
    "ui.blur_radius",
    "ui.blur.radius",
    "ui.background_style",
    "ui.background.style",
    "display.line_numbers",
    "display.line.numbers",
    "display.gutter",
    "display.word_count",
    "display.word.count",
    "display.comments",
    "command_runner.show_key_hints",
    "command.runner.show.key.hints",
    "command_runner.cursor_peek.enabled",
    "command.runner.cursor.peek.enabled",
    "command_runner.cursor_peek",
    "command.runner.cursor.peek",
    "command_runner.cursor_peek.modifier",
    "command.runner.cursor.peek.modifier",
    "command_runner.cursor_peek.tap_window_ms",
    "command.runner.cursor.peek.tap.window.ms",
    "command_runner.cursor_peek.placement",
    "command.runner.cursor.peek.placement",
    "display.pane_headers",
    "display.pane.headers",
    "display.focused_text_body",
    "display.focused.text.body",
    "display.contextual_toolbar",
    "display.contextual.toolbar",
    "display.contextual_toolbar_mode",
    "display.contextual.toolbar.mode",
    "text_area.left.percent",
    "text.area.left.percent",
    "text_area.right.percent",
    "text.area.right.percent",
    "text_area.top.percent",
    "text.area.top.percent",
    "text_area.bottom.percent",
    "text.area.bottom.percent",
    "viewport.width.percent",
    "viewport.width.max",
    "viewport.height.percent",
    "viewport.height.max"
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
    "ui_material"                              -> "ui.material",
    "material_preset"                          -> "material.preset",
    "ui_motion"                                -> "ui.motion",
    "motion_preset"                            -> "motion.preset",
    "ui_motion_speed_scale"                    -> "ui.motion.speed_scale",
    "motion_speed_scale"                       -> "motion.speed_scale",
    "ui_motion_editor_text_speed_scale"        -> "ui.motion.editor_text.speed_scale",
    "ui_motion_command_runner_speed_scale"     -> "ui.motion.command_runner.speed_scale",
    "ui_motion_ui_speed_scale"                 -> "ui.motion.ui.speed_scale",
    "ui_motion_cursor_speed_scale"             -> "ui.motion.cursor.speed_scale",
    "ui_motion_command_runner"                 -> "ui.motion.command_runner",
    "ui_motion_command_runner_reveal"          -> "ui.motion.command_runner_reveal",
    "ui_motion_ui"                             -> "ui.motion.ui",
    "ui_motion_editor_text"                    -> "ui.motion.editor_text",
    "ui_motion_panel_open"                     -> "ui.motion.panel_open",
    "ui_motion_panel_close"                    -> "ui.motion.panel_close",
    "command_runner_visible_rows"              -> "command_runner.visible_rows",
    "command_runner_item_gap_rows"             -> "command_runner.item_gap_rows",
    "command_runner_cursor_gap_rows"           -> "command_runner.cursor_gap_rows",
    "render_fps"                               -> "render.fps",
    "ui_render_fps"                            -> "ui.render.fps",
    "render_damage_granularity"                -> "render.damage_granularity",
    "display_cursor_info_bar_background_alpha" -> "display.cursor_info_bar_background_alpha",
    "display_word_wrap"                        -> "display.word_wrap",
    "display_visual_line_navigation"           -> "display.visual_line_navigation",
    "display_pane_headers"                     -> "display.pane_headers",
    "display_focused_text_body"                -> "display.focused_text_body",
    "display_contextual_toolbar"               -> "display.contextual_toolbar",
    "display_contextual_toolbar_mode"          -> "display.contextual_toolbar_mode",
    "text_area_left_percent"                   -> "text_area.left.percent",
    "text_area_right_percent"                  -> "text_area.right.percent",
    "text_area_top_percent"                    -> "text_area.top.percent",
    "text_area_bottom_percent"                 -> "text_area.bottom.percent",
    "viewport_width_percent"                   -> "viewport.width.percent",
    "viewport_width_max"                       -> "viewport.width.max",
    "viewport_height_percent"                  -> "viewport.height.percent",
    "viewport_height_max"                      -> "viewport.height.max"
  )

  val commandRunnerVisibleRowsKeys: Set[String] =
    Set("command_runner.visible_rows", "command.runner.visible.rows", "command_runner_visible_rows")

  val commandRunnerItemGapRowsKeys: Set[String] =
    Set("command_runner.item_gap_rows", "command.runner.item.gap.rows", "command_runner_item_gap_rows")

  val commandRunnerCursorGapRowsKeys: Set[String] =
    Set("command_runner.cursor_gap_rows", "command.runner.cursor.gap.rows", "command_runner_cursor_gap_rows")

  val renderFpsKeys: Set[String] = Set("render.fps", "render_fps", "ui.render.fps", "ui_render_fps")

  val renderDamageGranularityKeys: Set[String] =
    Set("render.damage_granularity", "render.damage.granularity", "render_damage_granularity")

  val materialPresetKeys: Set[String] = Set("ui.material", "ui_material", "material.preset", "material_preset")

  val postProcessingKeys: Set[String] = Set("ui.post_processing")

  val uiShadowsKeys: Set[String] = Set("ui.shadows", "ui_shadows")

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

  val cursorInfoBarBackgroundAlphaKeys: Set[String] =
    Set(
      "display.cursor_info_bar_background_alpha",
      "display.cursor_info_bar.background_alpha",
      "display_cursor_info_bar_background_alpha"
    )

  val wordWrapKeys: Set[String] = Set("display.word_wrap", "display.word.wrap", "display_word_wrap")

  val visualLineNavigationKeys: Set[String] =
    Set("display.visual_line_navigation", "display.visual.line.navigation", "display_visual_line_navigation")

  val blurRadiusKeys: Set[String] = Set("ui.blur_radius", "ui.blur.radius", "ui_blur_radius")

  val backgroundStyleKeys: Set[String] = Set("ui.background_style", "ui.background.style", "ui_background_style")

  val lineNumberKeys: Set[String] =
    Set("display.line_numbers", "display.line.numbers", "display_line_numbers")

  val gutterKeys: Set[String] = Set("display.gutter", "display_gutter")

  val wordCountKeys: Set[String] = Set("display.word_count", "display.word.count", "display_word_count")

  val commentDisplayModeKeys: Set[String] = Set("display.comments", "display_comments")

  val commandRunnerShowKeyHintsKeys: Set[String] =
    Set("command_runner.show_key_hints", "command.runner.show.key.hints", "command_runner_show_key_hints")

  /** `.enabled` rather than `command_runner.cursor_peek` itself: the peek settings below are children of that path, and
    * HOCON drops a leaf that also has children.
    */
  val commandRunnerCursorPeekKeys: Set[String] =
    Set(
      "command_runner.cursor_peek.enabled",
      "command.runner.cursor.peek.enabled",
      "command_runner.cursor_peek",
      "command.runner.cursor.peek",
      "command_runner_cursor_peek"
    )

  val commandRunnerCursorPeekModifierKeys: Set[String] =
    Set("command_runner.cursor_peek.modifier", "command.runner.cursor.peek.modifier")

  val commandRunnerCursorPeekTapWindowKeys: Set[String] =
    Set("command_runner.cursor_peek.tap_window_ms", "command.runner.cursor.peek.tap.window.ms")

  val commandRunnerCursorPeekPlacementKeys: Set[String] =
    Set("command_runner.cursor_peek.placement", "command.runner.cursor.peek.placement")

  val paneHeaderKeys: Set[String] =
    Set("display.pane_headers", "display.pane.headers", "display_pane_headers")

  val focusedTextBodyKeys: Set[String] =
    Set("display.focused_text_body", "display.focused.text.body", "display_focused_text_body")

  val contextualToolbarKeys: Set[String] =
    Set("display.contextual_toolbar", "display.contextual.toolbar", "display_contextual_toolbar")

  val contextualToolbarModeKeys: Set[String] =
    Set(
      "display.contextual_toolbar_mode",
      "display.contextual.toolbar.mode",
      "display_contextual_toolbar_mode"
    )

  val textAreaLeftPercentKeys: Set[String] =
    Set("text_area.left.percent", "text.area.left.percent", "text_area_left_percent")

  val textAreaRightPercentKeys: Set[String] =
    Set("text_area.right.percent", "text.area.right.percent", "text_area_right_percent")

  val textAreaTopPercentKeys: Set[String] =
    Set("text_area.top.percent", "text.area.top.percent", "text_area_top_percent")

  val textAreaBottomPercentKeys: Set[String] =
    Set("text_area.bottom.percent", "text.area.bottom.percent", "text_area_bottom_percent")

  val viewportWidthPercentKeys: Set[String] = Set("viewport.width.percent", "viewport_width_percent")

  val viewportWidthMaxKeys: Set[String] = Set("viewport.width.max", "viewport_width_max")

  val viewportHeightPercentKeys: Set[String] = Set("viewport.height.percent", "viewport_height_percent")

  val viewportHeightMaxKeys: Set[String] = Set("viewport.height.max", "viewport_height_max")

  private val handledKeys: Set[String] =
    materialPresetKeys ++
      postProcessingKeys ++
      uiShadowsKeys ++
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
      panelCloseTransitionKeys ++
      commandRunnerVisibleRowsKeys ++
      commandRunnerItemGapRowsKeys ++
      commandRunnerCursorGapRowsKeys ++
      cursorInfoBarBackgroundAlphaKeys ++
      renderFpsKeys ++
      renderDamageGranularityKeys ++
      wordWrapKeys ++
      blurRadiusKeys ++
      backgroundStyleKeys ++
      visualLineNavigationKeys ++
      lineNumberKeys ++
      gutterKeys ++
      wordCountKeys ++
      commentDisplayModeKeys ++
      commandRunnerShowKeyHintsKeys ++
      commandRunnerCursorPeekKeys ++
      commandRunnerCursorPeekModifierKeys ++
      commandRunnerCursorPeekTapWindowKeys ++
      commandRunnerCursorPeekPlacementKeys ++
      paneHeaderKeys ++
      focusedTextBodyKeys ++
      contextualToolbarKeys ++
      contextualToolbarModeKeys ++
      textAreaLeftPercentKeys ++
      textAreaRightPercentKeys ++
      textAreaTopPercentKeys ++
      textAreaBottomPercentKeys ++
      viewportWidthPercentKeys ++
      viewportWidthMaxKeys ++
      viewportHeightPercentKeys ++
      viewportHeightMaxKeys

  def handles(key: String): Boolean =
    handledKeys.contains(key)
