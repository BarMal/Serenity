package com.serenity.config

/** Static and dynamic keys understood by the text config format. */
object ConfigKeySchema:

  val dynamicPrefixes: List[String] = List(
    "lsp.",
    "hotkey.",
    "keymap.editor.",
    "keymap.command_runner.",
    "keymap.modal.",
    "keymap.panel.",
    "keymap.peek."
  )

  def deprecatedReplacement(key: String): Option[String] =
    deprecatedKeys.get(key)

  def isKnownKey(key: String): Boolean =
    currentKeys.contains(key) ||
      deprecatedKeys.contains(key) ||
      dynamicPrefixes.exists(key.startsWith)

  val currentKeys: Set[String] =
    Set(
      "config.version",
      "character.animation",
      "character.animation.duration_ms",
      "character.animation.duration.ms",
      "character.animation.steps",
      "syntax.highlighting",
      "font.code.family",
      "font.text.family",
      "font.ui.family",
      "font.code.size",
      "font.text.size",
      "font.prose.size",
      "font.ui.size",
      "font.scale.mode",
      "font.text_scale",
      "font.text.scale",
      "font.code.ligatures",
      "font.text.ligatures",
      "font.prose.ligatures",
      "font.ui.ligatures",
      "ui.material",
      "material.preset",
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
      "render.fps",
      "ui.render.fps",
      "display.word_wrap",
      "display.word.wrap",
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
      "viewport.height.max",
      "spellcheck.enabled",
      "spellcheck.languages",
      "spellcheck.dictionary_paths",
      "spellcheck.dictionary.paths",
      "spellcheck.words"
    ) ++
      SurfaceConfig.Schema.currentKeys ++
      CursorConfig.Schema.currentKeys ++
      DocumentConfig.Schema.currentKeys ++
      InterfaceConfig.Schema.currentKeys ++
      WindowConfig.Schema.currentKeys

  val deprecatedKeys: Map[String, String] =
    Map(
      "character_animation"                  -> "character.animation",
      "character_animation_duration_ms"      -> "character.animation.duration_ms",
      "character_animation_steps"            -> "character.animation.steps",
      "syntax_highlighting"                  -> "syntax.highlighting",
      "font_code_family"                     -> "font.code.family",
      "font_text_family"                     -> "font.text.family",
      "font_ui_family"                       -> "font.ui.family",
      "font_code_size"                       -> "font.code.size",
      "font_text_size"                       -> "font.text.size",
      "font_prose_size"                      -> "font.text.size",
      "font_size"                            -> "font.code.size and font.text.size",
      "font_ui_size"                         -> "font.ui.size",
      "font_scale_mode"                      -> "font.scale.mode",
      "font_text_scale"                      -> "font.text_scale",
      "font_code_ligatures"                  -> "font.code.ligatures",
      "font_text_ligatures"                  -> "font.text.ligatures",
      "font_prose_ligatures"                 -> "font.text.ligatures",
      "font_ligatures"                       -> "font.code.ligatures and font.text.ligatures",
      "font_ui_ligatures"                    -> "font.ui.ligatures",
      "ui_material"                          -> "ui.material",
      "material_preset"                      -> "material.preset",
      "ui_motion"                            -> "ui.motion",
      "motion_preset"                        -> "motion.preset",
      "ui_motion_speed_scale"                -> "ui.motion.speed_scale",
      "ui_motion_editor_text_speed_scale"    -> "ui.motion.editor_text.speed_scale",
      "ui_motion_command_runner_speed_scale" -> "ui.motion.command_runner.speed_scale",
      "ui_motion_ui_speed_scale"             -> "ui.motion.ui.speed_scale",
      "ui_motion_cursor_speed_scale"         -> "ui.motion.cursor.speed_scale",
      "ui_motion_command_runner"             -> "ui.motion.command_runner",
      "ui_motion_command_runner_reveal"      -> "ui.motion.command_runner_reveal",
      "ui_motion_ui"                         -> "ui.motion.ui",
      "ui_motion_editor_text"                -> "ui.motion.editor_text",
      "ui_motion_panel_open"                 -> "ui.motion.panel_open",
      "ui_motion_panel_close"                -> "ui.motion.panel_close",
      "motion_speed_scale"                   -> "motion.speed_scale",
      "command_runner_visible_rows"          -> "command_runner.visible_rows",
      "render_fps"                           -> "render.fps",
      "ui_render_fps"                        -> "ui.render.fps",
      "display_word_wrap"                    -> "display.word_wrap",
      "display_focused_text_body"            -> "display.focused_text_body",
      "display_contextual_toolbar"           -> "display.contextual_toolbar",
      "display_contextual_toolbar_mode"      -> "display.contextual_toolbar_mode",
      "text_area_left_percent"               -> "text_area.left.percent",
      "text_area_right_percent"              -> "text_area.right.percent",
      "text_area_top_percent"                -> "text_area.top.percent",
      "text_area_bottom_percent"             -> "text_area.bottom.percent",
      "viewport_width_percent"               -> "viewport.width.percent",
      "viewport_width_max"                   -> "viewport.width.max",
      "viewport_height_percent"              -> "viewport.height.percent",
      "viewport_height_max"                  -> "viewport.height.max",
      "spellcheck_enabled"                   -> "spellcheck.enabled",
      "spellcheck_languages"                 -> "spellcheck.languages",
      "spellcheck_dictionary_paths"          -> "spellcheck.dictionary_paths",
      "spellcheck_words"                     -> "spellcheck.words"
    ) ++
      SurfaceConfig.Schema.deprecatedKeys ++
      CursorConfig.Schema.deprecatedKeys ++
      DocumentConfig.Schema.deprecatedKeys ++
      InterfaceConfig.Schema.deprecatedKeys ++
      WindowConfig.Schema.deprecatedKeys
