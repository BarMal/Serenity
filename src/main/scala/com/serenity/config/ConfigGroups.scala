package com.serenity.config

import com.serenity.animation.{AnimationConfig, TransitionKind, TransitionScope}
import com.serenity.lsp.config.LspUserConfig
import com.typesafe.config.ConfigUtil

/** The settings that are not one key to one value.
  *
  * An animation preset spreads over three keys; a motion family over six; the LSP, hotkey and keymap groups have as
  * many keys as there are languages, actions and bindings. They cannot be [[ConfigField]]s, but they are still settings
  * that have to be written, read and covered -- so they are declared here in the same shape, and the writer, the parser
  * and the coverage tests treat a group exactly as they treat a field.
  */
object ConfigGroups:

  private val motionFamilyPrefix = "ui.motion.family."

  val dynamicPrefixes: List[String] =
    List("lsp.", "hotkey.", "keymap.", motionFamilyPrefix)

  def animationPresetName(animation: Option[AnimationConfig]): String =
    animation match
      case None                                                 => "none"
      case Some(anim) if anim == AnimationConfig.Enabled.quick  => "quick"
      case Some(anim) if anim == AnimationConfig.Enabled.smooth => "smooth"
      case Some(anim) if anim == AnimationConfig.Enabled.subtle => "subtle"
      case Some(_)                                              => "custom"

  def transitionKindConfigKey(kind: TransitionKind): String =
    kind match
      case TransitionKind.Disabled               => "off"
      case TransitionKind.Fade                   => "fade"
      case TransitionKind.TypedText              => "typed"
      case TransitionKind.DirectionalSweep       => "directional"
      case TransitionKind.OutlineThenContent     => "outline"
      case TransitionKind.LineAndCharacterTandem => "tandem"

  /** A preset name, plus the two keys that only a custom preset needs. */
  def animationEntries(prefix: String, animation: Option[AnimationConfig]): List[(String, HoconValue)] =
    val preset = animationPresetName(animation)
    val custom =
      if preset != "custom" then Nil
      else
        animation.toList.flatMap { anim =>
          List(
            s"$prefix.duration_ms" -> HoconValue.number(anim.durationMs),
            s"$prefix.steps"       -> HoconValue.number(anim.steps)
          )
        }
    (s"$prefix.preset" -> HoconValue.string(preset)) :: custom

  def characterAnimation(config: AppConfig): List[(String, HoconValue)] =
    animationEntries("character.animation", config.editorConfig.characterAnimation)

  /** The motion hierarchy, plus the legacy per-family speed scales that still override it where they are set. */
  def motion(config: AppConfig): List[(String, HoconValue)] =
    val surface = config.surfaceConfig
    val settings = surface.motionConfiguration match
      case Some(configuration) => configuration.withFallback(MotionConfig.fromLegacy(surface, configuration.baseline))
      case None                => MotionConfig.fromLegacy(surface)

    val legacySpeeds = List(
      "editor_text"    -> surface.editorTextTransitionSpeedScale,
      "command_runner" -> surface.commandRunnerTransitionSpeedScale,
      "ui"             -> surface.uiTransitionSpeedScale,
      "cursor"         -> surface.cursorTransitionSpeedScale
    ).collect { case (name, Some(value)) => s"ui.motion.$name.speed_scale" -> HoconValue.number(value) }

    val families = MotionFamily.values.toList.flatMap { family =>
      val family_ = settings.families(family)
      val prefix  = s"$motionFamilyPrefix${family.configKey}"
      val speedScale = family match
        case MotionFamily.EditorText      => surface.editorTextTransitionSpeedScale.getOrElse(family_.speedScale)
        case MotionFamily.CommandSurfaces => surface.commandRunnerTransitionSpeedScale.getOrElse(family_.speedScale)
        case MotionFamily.UiTransitions   => surface.uiTransitionSpeedScale.getOrElse(family_.speedScale)
        case MotionFamily.Cursor          => surface.cursorTransitionSpeedScale.getOrElse(family_.speedScale)
        case MotionFamily.PinnedPanels    => family_.speedScale
      val scopedTransitions =
        if family != MotionFamily.PinnedPanels then Nil
        else
          List(
            s"$prefix.open_transition" ->
              HoconValue.string(transitionKindConfigKey(family_.transitionKindFor(TransitionScope.PanelOpen))),
            s"$prefix.close_transition" ->
              HoconValue.string(transitionKindConfigKey(family_.transitionKindFor(TransitionScope.PanelClose)))
          )

      List(
        s"$prefix.enabled"    -> HoconValue.boolean(family_.enabled),
        s"$prefix.transition" -> HoconValue.string(transitionKindConfigKey(family_.transitionKind))
      ) ++ animationEntries(s"$prefix.animation", family_.animation) ++
        List(s"$prefix.speed_scale" -> HoconValue.number(speedScale)) ++ scopedTransitions
    }

    List(
      "ui.motion.preset"        -> HoconValue.string(settings.baseline.configKey),
      "ui.motion.accessibility" -> HoconValue.string(settings.accessibility.configKey)
    ) ++ legacySpeeds ++ families

  def lsp(config: LspUserConfig): List[(String, HoconValue)] =
    config.servers
      .getOrElse(Map.empty)
      .toList
      .sortBy(_._1)
      .flatMap {
        case (languageId, serverOverride) =>
          def key(field: String) = ConfigUtil.joinPath("lsp", languageId, field)
          List(
            serverOverride.enabled.map(enabled => key("enabled") -> HoconValue.boolean(enabled)),
            serverOverride.command.map(command => key("command") -> HoconValue.string(command)),
            serverOverride.args.map(args => key("args") -> HoconValue.list(args))
          ).flatten
      }

  def hotkeys(config: AppConfig): List[(String, HoconValue)] =
    HotkeyAction.values.toList.map { action =>
      ConfigUtil.joinPath("hotkey", action.configKey) ->
        HoconValue.list(config.inputConfig.hotkeyConfig.bindingsFor(action).map(_.render))
    }

  def keymaps(config: AppConfig): List[(String, HoconValue)] =
    val keymap = config.inputConfig.focusedKeymapConfig

    def binding(bindings: List[HotkeyTrigger], defaults: List[HotkeyTrigger]): HoconValue =
      HoconValue.string(bindings.headOption.orElse(defaults.headOption).fold("")(_.render))

    def editor(action: EditorKeyAction): HoconValue =
      binding(keymap.editor.bindingsFor(action), EditorKeyAction.defaultBindings.getOrElse(action, Nil))

    List(
      "keymap.editor.page_down"              -> editor(EditorKeyAction.PageDown),
      "keymap.editor.extend_selection_left"  -> editor(EditorKeyAction.ExtendSelectionLeft),
      "keymap.editor.extend_selection_right" -> editor(EditorKeyAction.ExtendSelectionRight),
      "keymap.editor.extend_selection_up"    -> editor(EditorKeyAction.ExtendSelectionUp),
      "keymap.editor.extend_selection_down"  -> editor(EditorKeyAction.ExtendSelectionDown),
      "keymap.command_runner.submit" -> binding(
        keymap.commandRunner.bindingsFor(CommandRunnerKeyAction.Submit),
        CommandRunnerKeyAction.defaultBindings.getOrElse(CommandRunnerKeyAction.Submit, Nil)
      ),
      "keymap.modal.dismiss" -> binding(
        keymap.modal.bindingsFor(ModalKeyAction.Dismiss),
        ModalKeyAction.defaultBindings.getOrElse(ModalKeyAction.Dismiss, Nil)
      )
    )

  /** The spellings groups own: the current one first, then the older ones that are still read.
    *
    * `ui.motion` is a leaf on a path whose children (`ui.motion.family`, `ui.motion.accessibility`) HOCON would resolve
    * by dropping it, so it survived only by being written as a quoted key. It stays readable for files that have it.
    */
  private val spellings: List[(String, Set[String])] = List(
    "character.animation.preset"      -> Set("character.animation", "character_animation"),
    "character.animation.duration_ms" -> Set("character.animation.duration.ms", "character_animation_duration_ms"),
    "character.animation.steps"       -> Set("character_animation_steps"),
    "ui.motion.preset"                -> Set("ui.motion", "ui_motion", "motion.preset", "motion_preset"),
    "ui.motion.accessibility"         -> Set.empty,
    "ui.motion.speed_scale"           -> Set("motion.speed_scale", "ui_motion_speed_scale", "motion_speed_scale"),
    "ui.motion.editor_text.speed_scale" ->
      Set("ui.motion.editor.text.speed_scale", "ui_motion_editor_text_speed_scale"),
    "ui.motion.command_runner.speed_scale" ->
      Set("ui.motion.command.runner.speed_scale", "ui_motion_command_runner_speed_scale"),
    "ui.motion.ui.speed_scale" ->
      Set("ui.motion.ui_elements.speed_scale", "ui.motion.ui.elements.speed_scale", "ui_motion_ui_speed_scale"),
    "ui.motion.cursor.speed_scale" ->
      Set("ui.motion.cursor_speed_scale", "ui.motion.cursor.speed.scale", "ui_motion_cursor_speed_scale"),
    "ui.motion.command_runner" -> Set("ui.motion.command.runner", "ui_motion_command_runner"),
    "ui.motion.command_runner_reveal" ->
      Set("ui.motion.command.runner.reveal", "ui_motion_command_runner_reveal"),
    "ui.motion.ui"          -> Set("ui.motion.ui_elements", "ui.motion.ui.elements", "ui_motion_ui"),
    "ui.motion.editor_text" -> Set("ui.motion.editor.text", "ui_motion_editor_text"),
    "ui.motion.panel_open"  -> Set("ui.motion.panel.open", "ui_motion_panel_open"),
    "ui.motion.panel_close" -> Set("ui.motion.panel.close", "ui_motion_panel_close")
  )

  private val familyKeys: Set[String] =
    MotionFamily.values.flatMap { family =>
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

  /** The static keys groups own, which is what keeps them out of "unknown key" warnings. */
  val currentKeys: Set[String] = spellings.map(_._1).toSet ++ familyKeys

  val deprecatedKeys: Map[String, String] =
    spellings.flatMap { case (current, older) => older.map(_ -> current) }.toMap

  def handles(key: String): Boolean =
    currentKeys.contains(key) || deprecatedKeys.contains(key) || dynamicPrefixes.exists(key.startsWith)
