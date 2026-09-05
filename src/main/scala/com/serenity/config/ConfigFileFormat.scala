package com.serenity.config

import java.util.Locale

import scala.jdk.CollectionConverters.*

import com.serenity.animation.{AnimationConfig, TransitionKind, TransitionScope}
import com.serenity.lsp.config.LspUserConfig
import com.serenity.ui.theme.ColorFormat
import com.typesafe.config.{ConfigFactory, ConfigRenderOptions, ConfigUtil, ConfigValue, ConfigValueFactory}

/** The configuration file, as data rather than as text.
  *
  * Every line is an [[Entry]]: a comment, a blank, or a setting carrying a [[Value]]. A `Value` holds the config
  * library's own representation alongside the text the file will carry, and the two are the reason this is not built
  * by interpolating strings:
  *
  *   - the text is derived from the library's rendering, so quoting and escaping are its problem. A font family
  *     containing a quote, a segment list containing a comma -- the cases that produced a file the parser then
  *     rejected, taking every other setting in it down to defaults -- cannot be written wrongly here;
  *   - the library representations are assembled into a `Config` before anything is written, which is what catches a
  *     key written at a path that also has children. HOCON resolves that by dropping the value, so assembling first
  *     turns it into a refused save rather than a setting that quietly disappears on the next launch.
  *
  * The layout -- order, blank lines, the comments explaining what each setting takes -- is authored here because the
  * library's renderer cannot emit those comments without also emitting its own (`# hardcoded value` against every
  * line). A file people are expected to open and edit is worth the few lines that keeps.
  */
object ConfigFileFormat:

  enum Entry:
    case Comment(text: String)
    case Blank
    case Setting(key: String, value: Value)

  /** A setting's value, in both the forms this needs it: the library's own, which decides whether the file can be read
    * back, and the text the file carries, which the library derives but does not choose the layout of.
    */
  final case class Value(config: ConfigValue, rendered: String)

  def render(config: AppConfig): String =
    entries(config)
      .map {
        case Entry.Comment(text)       => s"# $text"
        case Entry.Blank               => ""
        case Entry.Setting(key, value) => s"$key = ${value.rendered}"
      }
      .mkString("", "\n", "\n")

  /** The settings [[render]] would emit that reading the file back would not return, empty when there are none.
    *
    * A key at a path that also has children (`ui.motion` alongside `ui.motion.family.…`) is not an error to the
    * library -- the later assignment simply replaces the earlier value with an object -- so assembling the settings
    * into a `Config` and counting what survives is what reveals it. A duplicated key shows up the same way.
    */
  def unwritableSettings(config: AppConfig): List[String] =
    val settings = entries(config).collect { case Entry.Setting(key, value) => key -> value.config }
    val assembled = settings.foldLeft(ConfigFactory.empty()) { case (acc, (key, value)) => acc.withValue(key, value) }
    if assembled.entrySet().size == settings.size then Nil
    else
      val surviving = assembled.entrySet().asScala.map(_.getKey).toSet
      settings.map(_._1).filterNot(surviving.contains).distinct

  /** The bare spelling where it is both readable and means the same thing, the library's quoted one otherwise.
    *
    * Most config values (`breathe`, `frosted`, `off`) read better unquoted, and that is the spelling this file has
    * always used. Two things have to hold for the quotes to go, and they answer different questions: the characters
    * decide whether a reader would find the bare form clearer, and parsing the bare form back decides whether it still
    * says the same thing. The second is what a character rule alone got wrong -- an unquoted comma turned a segment
    * list into a file the parser rejected, taking every other setting in it down to defaults.
    */
  private def string(value: String): Value =
    val quoted = ConfigValueFactory.fromAnyRef(value).render(ConfigRenderOptions.concise())
    val legible = value.nonEmpty && value.forall(char => char.isLetterOrDigit || "_./-".contains(char))
    val faithful =
      legible && (try ConfigFactory.parseString(s"probe = $value").getString("probe") == value
      catch case _: Exception => false)
    Value(ConfigValueFactory.fromAnyRef(value), if faithful then value else quoted)

  /** Numbers are written as Scala spells them; the library keeps the text it parsed, so the two agree.
    *
    * `fromAnyRef` would not: it renders a `Double` of 12 as `12`, and widens a `Float` of 0.18f to
    * `0.18000000715255737`.
    */
  private def number(value: Double): Value = numberText(value.toString)
  private def number(value: Float): Value  = numberText(value.toString)
  private def number(value: Int): Value    = numberText(value.toString)
  private def number(value: Long): Value   = numberText(value.toString)

  private def numberText(text: String): Value =
    Value(ConfigFactory.parseString(s"probe = $text").getValue("probe"), text)

  private def boolean(value: Boolean): Value =
    Value(ConfigValueFactory.fromAnyRef(value), value.toString)

  /** List elements are always quoted: they are free text, and a bare one would run into the separator. */
  private def list(values: List[String]): Value =
    Value(
      ConfigValueFactory.fromAnyRef(values.asJava),
      values
        .map(value => ConfigValueFactory.fromAnyRef(value).render(ConfigRenderOptions.concise()))
        .mkString("[", ", ", "]")
    )

  /** `auto` is how this file spells "no value set" for the settings that have such a state. */
  private def optionalNumber(value: Option[Double]): Value = value.fold(string("auto"))(number)
  private def optionalCount(value: Option[Int]): Value      = value.fold(string("auto"))(number)

  /** An empty string is how it spells the same thing for the ones that predate that convention. */
  private def optionalString(value: Option[String]): Value = string(value.getOrElse(""))

  private def animationPreset(animation: Option[AnimationConfig]): String =
    animation match
      case None                                                 => "none"
      case Some(anim) if anim == AnimationConfig.Enabled.quick  => "quick"
      case Some(anim) if anim == AnimationConfig.Enabled.smooth => "smooth"
      case Some(anim) if anim == AnimationConfig.Enabled.subtle => "subtle"
      case Some(_)                                              => "custom"

  private def transitionKindConfigKey(kind: TransitionKind): String =
    kind match
      case TransitionKind.Disabled              => "off"
      case TransitionKind.Fade                  => "fade"
      case TransitionKind.TypedText             => "typed"
      case TransitionKind.DirectionalSweep      => "directional"
      case TransitionKind.OutlineThenContent    => "outline"
      case TransitionKind.LineAndCharacterTandem => "tandem"

  private def entries(config: AppConfig): List[Entry] =
    val surface   = config.surfaceConfig
    val fonts     = config.editorConfig.fontConfig
    val sitter    = config.windowSitterConfig
    val spellings = config.languageToolsConfig.spellCheck.normalized

    List(
      Entry.Comment("Serenity Editor Configuration"),
      Entry.Setting("config.version", number(ConfigVersion.Current.value)),
      Entry.Blank,
      Entry.Comment("Character animation style: none, quick, smooth, subtle, custom"),
      Entry.Setting("character.animation.preset", string(animationPreset(config.editorConfig.characterAnimation)))
    ) ++ customAnimation("character.animation", config.editorConfig.characterAnimation) ++ List(
      Entry.Blank,
      Entry.Comment("Syntax highlighting: true, false"),
      Entry.Setting("syntax.highlighting", boolean(config.languageToolsConfig.syntaxHighlightingEnabled)),
      Entry.Blank,
      Entry.Comment("Font configuration"),
      Entry.Setting("font.code.family", string(fonts.codeFontFamily)),
      Entry.Setting("font.text.family", string(fonts.textFontFamily)),
      Entry.Setting("font.ui.family", string(fonts.uiFontFamily)),
      Entry.Setting("font.code.size", number(fonts.codeFontSize)),
      Entry.Setting("font.text.size", number(fonts.textFontSize)),
      Entry.Setting("font.ui.size", number(fonts.uiFontSize)),
      Entry.Setting("font.scale.mode", string(fonts.textScaleMode.configKey)),
      Entry.Setting("font.text_scale", number(fonts.textScaleMultiplier)),
      Entry.Setting("font.code.ligatures", boolean(fonts.codeLigatures)),
      Entry.Setting("font.text.ligatures", boolean(fonts.textLigatures)),
      Entry.Setting("font.ui.ligatures", boolean(fonts.uiLigatures)),
      Entry.Blank,
      Entry.Comment("Cursor colour overrides. Leave empty to use the active theme cursor."),
      Entry.Setting("cursor.mode", string(config.cursorMode.configKey)),
      Entry.Setting("cursor.active.color", optionalString(config.cursorColors.active.map(formatColor))),
      Entry.Setting("cursor.inactive.color", optionalString(config.cursorColors.inactive.map(formatColor))),
      Entry.Comment(
        "Comma-separated segment list (title, position, word_count, char_count, reading_time), or off/empty to hide"
      ),
      Entry.Setting(
        "cursor.info_bar.segments",
        string(
          if config.cursorInfoBarSegments.isEmpty then "off"
          else config.cursorInfoBarSegments.map(_.configKey).mkString(",")
        )
      ),
      Entry.Setting("cursor.info_bar.placement", string(config.cursorInfoBarPlacement.configKey)),
      Entry.Blank,
      Entry.Comment("Interface density: compact, comfortable, spacious"),
      Entry.Setting("interface.density", string(config.interfaceDensity.configKey)),
      Entry.Comment(
        "Window chrome: auto uses themed chrome on Linux; native preserves OS snap/window animations; native-themed " +
          "uses Windows system chrome colours; custom is themed and applies after restart"
      ),
      Entry.Setting("window.chrome", string(config.windowChromeMode.configKey)),
      Entry.Setting("window.sitter.enabled", boolean(sitter.enabled)),
      Entry.Setting("window.sitter.action", string(sitter.action.configKey)),
      Entry.Setting("window.sitter.frames", list(sitter.frames.toList)),
      Entry.Setting("window.sitter.active_ticks", number(sitter.activeTicks)),
      Entry.Setting("window.sitter.fast_active_ticks", number(sitter.fastActiveTicks)),
      Entry.Setting("window.sitter.fast_typing_threshold_ms", number(sitter.fastTypingThresholdMs)),
      Entry.Setting("ui.element_gap", number(config.uiElementGap)),
      Entry.Setting("ui.corner_radius", number(config.uiCornerRadiusPx)),
      Entry.Setting("ui.outline_thickness", number(config.uiOutlineThicknessPx)),
      Entry.Setting("command_runner.visible_rows", optionalCount(surface.commandRunnerVisibleRows)),
      Entry.Setting("command_runner.item_gap_rows", number(surface.commandRunnerItemGapRows)),
      Entry.Setting("command_runner.cursor_gap_rows", optionalNumber(surface.commandRunnerCursorGapRows)),
      Entry.Setting("command_runner.show_key_hints", boolean(surface.commandRunnerShowKeyHints)),
      Entry.Comment("Hold or double-tap a bare modifier to peek the command runner near the cursor"),
      Entry.Setting("command_runner.cursor_peek.enabled", boolean(surface.commandRunnerCursorPeekEnabled)),
      Entry.Setting(
        "command_runner.cursor_peek.modifier",
        string(surface.commandRunnerCursorPeekModifier.toString.toLowerCase(Locale.ROOT))
      ),
      Entry.Setting("command_runner.cursor_peek.tap_window_ms", number(surface.commandRunnerCursorPeekTapWindowMillis)),
      Entry.Setting(
        "command_runner.cursor_peek.placement",
        string(surface.commandRunnerCursorPeekPlacement.toString.toLowerCase(Locale.ROOT))
      ),
      Entry.Setting("render.fps", string(surface.renderFpsTarget.configKey)),
      Entry.Comment(
        "Damage granularity the renderer honours: rows redraws whole visible lines; cells honours column ranges on"
      ),
      Entry.Comment("monospaced buffers only, falling back to rows for proportional or ligature-shaped text"),
      Entry.Setting("render.damage_granularity", string(surface.renderDamageGranularity.configKey)),
      Entry.Comment(
        "Cursor info bar background alpha (0.0-1.0), overriding the active theme's own panel alpha for just that"
      ),
      Entry.Comment("panel. auto/default keeps the theme's alpha."),
      Entry.Setting(
        "display.cursor_info_bar_background_alpha",
        optionalNumber(surface.cursorInfoBarBackgroundAlpha)
      ),
      Entry.Setting("display.word_wrap", boolean(surface.wordWrapEnabled)),
      Entry.Setting("display.visual_line_navigation", boolean(surface.visualLineCursorNavigation)),
      Entry.Setting("display.line_numbers", boolean(surface.showLineNumbers)),
      Entry.Setting("display.gutter", boolean(surface.showGutter)),
      Entry.Setting("display.word_count", boolean(surface.showWordCount)),
      Entry.Comment("Where document comments are shown: floating, margin"),
      Entry.Setting("display.comments", string(surface.commentDisplayMode.configKey)),
      Entry.Setting("display.pane_headers", boolean(surface.showPaneHeaders)),
      Entry.Setting("display.focused_text_body", boolean(surface.focusedTextBodyEnabled)),
      Entry.Setting("display.contextual_toolbar", boolean(surface.contextualToolbarEnabled)),
      Entry.Setting("display.contextual_toolbar_mode", string(surface.contextualToolbarDisplayMode.configKey)),
      Entry.Blank,
      Entry.Comment(
        "UI material and motion presets: solid, clear, frosted, crystal, custom / reduced, subtle, smooth, " +
          "expressive, custom"
      ),
      Entry.Setting("ui.material", string(surface.materialPreset.configKey)),
      Entry.Comment("Post-processing: off, scanlines, glow, scanlines-glow"),
      Entry.Setting("ui.post_processing", string(surface.postProcessingEffect.configKey)),
      Entry.Comment("Draw soft shadows behind menus and panels"),
      Entry.Setting("ui.shadows", boolean(surface.uiShadowsEnabled)),
      Entry.Comment("Background treatment behind panes: solid, transparent, frosted, glass-like"),
      Entry.Setting("ui.background_style", string(surface.backgroundStyle.configKey)),
      Entry.Comment("Blur strength behind translucent surfaces (0.0-1.0)"),
      Entry.Setting("ui.blur_radius", number(surface.blurRadius))
    ) ++ motionEntries(config) ++ List(
      Entry.Blank,
      Entry.Comment("Markdown rendering mode: source, split-preview, inline-lens"),
      Entry.Setting("document.markdown_view", string(config.markdownViewMode.configKey)),
      Entry.Blank,
      Entry.Comment("Default mode for new buffers: plain-text, markdown, rich-text"),
      Entry.Setting("document.default_mode", string(config.defaultDocumentMode.configKey)),
      Entry.Blank,
      Entry.Setting("editor.minimum_pane_width", number(config.editorConfig.minimumPaneWidth)),
      Entry.Comment("Lines one mouse-wheel notch scrolls"),
      Entry.Setting("input.wheel_scroll_lines", number(config.inputConfig.wheelScrollLines)),
      Entry.Blank,
      Entry.Comment("Preferred desktop window size. Leave empty to use the default."),
      Entry.Setting("window.preferred.width", optionalString(config.preferredWindowSize.map(_.width.toString))),
      Entry.Setting("window.preferred.height", optionalString(config.preferredWindowSize.map(_.height.toString))),
      Entry.Blank,
      Entry.Comment("Text area insets as percentages of the central workspace."),
      Entry.Setting("text_area.left.percent", number(surface.textAreaInsets.leftPercent)),
      Entry.Setting("text_area.right.percent", number(surface.textAreaInsets.rightPercent)),
      Entry.Setting("text_area.top.percent", number(surface.textAreaInsets.topPercent)),
      Entry.Setting("text_area.bottom.percent", number(surface.textAreaInsets.bottomPercent)),
      Entry.Setting("viewport.width.percent", number(surface.viewportSizing.width.percentValue)),
      Entry.Setting("viewport.width.max", optionalString(surface.viewportSizing.width.maxCells.map(_.toString))),
      Entry.Setting("viewport.height.percent", number(surface.viewportSizing.height.percentValue)),
      Entry.Setting("viewport.height.max", optionalString(surface.viewportSizing.height.maxCells.map(_.toString))),
      Entry.Blank,
      Entry.Comment("LSP server overrides")
    ) ++ lspEntries(config.languageToolsConfig.lspUserConfig) ++ List(
      Entry.Blank,
      Entry.Comment("Spell-checking for prose buffers"),
      Entry.Setting("spellcheck.enabled", boolean(config.languageToolsConfig.spellCheck.enabled)),
      Entry.Setting("spellcheck.languages", list(spellings.languages)),
      Entry.Setting("spellcheck.dictionary_paths", list(spellings.dictionaryPaths)),
      Entry.Setting("spellcheck.words", list(spellings.additionalWords)),
      Entry.Blank,
      Entry.Comment("Hotkey overrides")
    ) ++ hotkeyEntries(config) ++ List(
      Entry.Blank,
      Entry.Comment("Focused keymap overrides")
    ) ++ keymapEntries(config)

  private def customAnimation(prefix: String, animation: Option[AnimationConfig]): List[Entry] =
    if animationPreset(animation) != "custom" then Nil
    else
      animation.toList.flatMap { anim =>
        List(
          Entry.Setting(s"$prefix.duration_ms", number(anim.durationMs)),
          Entry.Setting(s"$prefix.steps", number(anim.steps))
        )
      }

  /** The motion hierarchy, plus the legacy per-family speed scales that still override it where they are set. */
  private def motionEntries(config: AppConfig): List[Entry] =
    val surface = config.surfaceConfig
    val motion = surface.motionConfiguration match
      case Some(configuration) => configuration.withFallback(MotionConfig.fromLegacy(surface, configuration.baseline))
      case None                => MotionConfig.fromLegacy(surface)

    val legacySpeeds = List(
      "editor_text"    -> surface.editorTextTransitionSpeedScale,
      "command_runner" -> surface.commandRunnerTransitionSpeedScale,
      "ui"             -> surface.uiTransitionSpeedScale,
      "cursor"         -> surface.cursorTransitionSpeedScale
    ).collect { case (key, Some(value)) => Entry.Setting(s"ui.motion.$key.speed_scale", number(value)) }

    val families = MotionFamily.values.toList.flatMap { family =>
      val settings = motion.families(family)
      val prefix   = s"ui.motion.family.${family.configKey}"
      val speedScale = family match
        case MotionFamily.EditorText      => surface.editorTextTransitionSpeedScale.getOrElse(settings.speedScale)
        case MotionFamily.CommandSurfaces => surface.commandRunnerTransitionSpeedScale.getOrElse(settings.speedScale)
        case MotionFamily.UiTransitions   => surface.uiTransitionSpeedScale.getOrElse(settings.speedScale)
        case MotionFamily.Cursor          => surface.cursorTransitionSpeedScale.getOrElse(settings.speedScale)
        case MotionFamily.PinnedPanels    => settings.speedScale
      val scopedTransitions =
        if family != MotionFamily.PinnedPanels then Nil
        else
          List(
            Entry.Setting(
              s"$prefix.open_transition",
              string(transitionKindConfigKey(settings.transitionKindFor(TransitionScope.PanelOpen)))
            ),
            Entry.Setting(
              s"$prefix.close_transition",
              string(transitionKindConfigKey(settings.transitionKindFor(TransitionScope.PanelClose)))
            )
          )

      List(
        Entry.Setting(s"$prefix.enabled", boolean(settings.enabled)),
        Entry.Setting(s"$prefix.transition", string(transitionKindConfigKey(settings.transitionKind))),
        Entry.Setting(s"$prefix.animation.preset", string(animationPreset(settings.animation)))
      ) ++ customAnimation(s"$prefix.animation", settings.animation) ++
        List(Entry.Setting(s"$prefix.speed_scale", number(speedScale))) ++ scopedTransitions
    }

    List(
      Entry.Setting("ui.motion.preset", string(motion.baseline.configKey)),
      Entry.Setting("ui.motion.accessibility", string(motion.accessibility.configKey))
    ) ++ legacySpeeds ++ families

  private def lspEntries(config: LspUserConfig): List[Entry] =
    config.servers
      .getOrElse(Map.empty)
      .toList
      .sortBy(_._1)
      .flatMap { case (languageId, serverOverride) =>
        def key(field: String) = ConfigUtil.joinPath("lsp", languageId, field)
        List(
          serverOverride.enabled.map(enabled => Entry.Setting(key("enabled"), boolean(enabled))),
          serverOverride.command.map(command => Entry.Setting(key("command"), string(command))),
          serverOverride.args.map(args => Entry.Setting(key("args"), list(args)))
        ).flatten
      }

  private def hotkeyEntries(config: AppConfig): List[Entry] =
    HotkeyAction.values.toList.map { action =>
      Entry.Setting(
        ConfigUtil.joinPath("hotkey", action.configKey),
        list(config.inputConfig.hotkeyConfig.bindingsFor(action).map(_.render))
      )
    }

  private def keymapEntries(config: AppConfig): List[Entry] =
    val keymap = config.inputConfig.focusedKeymapConfig

    def binding(bindings: List[HotkeyTrigger], defaults: List[HotkeyTrigger]): Value =
      string(bindings.headOption.orElse(defaults.headOption).fold("")(_.render))

    def editor(action: EditorKeyAction): Value =
      binding(keymap.editor.bindingsFor(action), EditorKeyAction.defaultBindings.getOrElse(action, Nil))

    List(
      Entry.Setting("keymap.editor.page_down", editor(EditorKeyAction.PageDown)),
      Entry.Setting("keymap.editor.extend_selection_left", editor(EditorKeyAction.ExtendSelectionLeft)),
      Entry.Setting("keymap.editor.extend_selection_right", editor(EditorKeyAction.ExtendSelectionRight)),
      Entry.Setting("keymap.editor.extend_selection_up", editor(EditorKeyAction.ExtendSelectionUp)),
      Entry.Setting("keymap.editor.extend_selection_down", editor(EditorKeyAction.ExtendSelectionDown)),
      Entry.Setting(
        "keymap.command_runner.submit",
        binding(
          keymap.commandRunner.bindingsFor(CommandRunnerKeyAction.Submit),
          CommandRunnerKeyAction.defaultBindings.getOrElse(CommandRunnerKeyAction.Submit, Nil)
        )
      ),
      Entry.Setting(
        "keymap.modal.dismiss",
        binding(
          keymap.modal.bindingsFor(ModalKeyAction.Dismiss),
          ModalKeyAction.defaultBindings.getOrElse(ModalKeyAction.Dismiss, Nil)
        )
      )
    )

  private def formatColor(color: java.awt.Color): String = ColorFormat.toHex(color, withAlpha = true)
