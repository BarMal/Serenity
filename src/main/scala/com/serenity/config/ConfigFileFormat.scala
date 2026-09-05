package com.serenity.config

import scala.jdk.CollectionConverters.*

import com.typesafe.config.ConfigFactory

/** The configuration file's layout: what order the settings appear in, and what the comments around them say.
  *
  * What each setting is called and how its value converts lives in [[ConfigRegistry]] and [[ConfigGroups]]; this file
  * only decides where it goes on the page. That split is the point -- a setting used to be able to exist in the parser
  * and not here, and a reader has no way to notice a line that was never written. Now the layout names fields, a field
  * it does not name fails [[missingFromLayout]], and the writer cannot silently drop one.
  *
  * The comments and ordering are authored rather than generated because the config library's own renderer cannot emit
  * them without also emitting its own (`# hardcoded value` against every line), and this is a file people open and
  * edit.
  */
object ConfigFileFormat:

  enum Entry:
    case Comment(text: String)
    case Blank
    case Field(key: String)
    case Group(settings: AppConfig => List[(String, HoconValue)])

  def render(config: AppConfig): String =
    lines(config)
      .map {
        case Left(comment)       => comment
        case Right((key, value)) => s"$key = ${value.rendered}"
      }
      .mkString("", "\n", "\n")

  /** The settings [[render]] would emit that reading the file back would not return, empty when there are none.
    *
    * A key at a path that also has children (`ui.motion` alongside `ui.motion.family.…`) is not an error to the library
    * -- the later assignment simply replaces the earlier value with an object -- so assembling the settings into a
    * `Config` and counting what survives is what reveals it. A duplicated key shows up the same way.
    */
  def unwritableSettings(config: AppConfig): List[String] =
    val settings  = lines(config).collect { case Right((key, value)) => key -> value.config }
    val assembled = settings.foldLeft(ConfigFactory.empty()) { case (acc, (key, value)) => acc.withValue(key, value) }
    if assembled.entrySet().size == settings.size then Nil
    else
      val surviving = assembled.entrySet().asScala.map(_.getKey).toSet
      settings.map(_._1).filterNot(surviving.contains).distinct

  /** Registered settings this layout would never write. Empty is the only acceptable answer; a test says so. */
  def missingFromLayout: List[String] =
    val laidOut = layout.collect { case Entry.Field(key) => key }.toSet
    ConfigRegistry.writtenKeys.filterNot(laidOut.contains)

  /** Keys the layout names that no longer exist. Also empty, also checked. */
  def unknownInLayout: List[String] =
    layout.collect { case Entry.Field(key) if ConfigRegistry.find(key).isEmpty => key }

  private def lines(config: AppConfig): List[Either[String, (String, HoconValue)]] =
    layout.flatMap {
      case Entry.Comment(text)   => List(Left(s"# $text"))
      case Entry.Blank           => List(Left(""))
      case Entry.Group(settings) => settings(config).map(Right.apply)
      case Entry.Field(key) => ConfigRegistry.find(key).toList.map(configField => Right(configField.setting(config)))
    }

  private def field(key: String): Entry    = Entry.Field(key)
  private def comment(text: String): Entry = Entry.Comment(text)
  private def blank: Entry                 = Entry.Blank

  private def group(settings: AppConfig => List[(String, HoconValue)]): Entry = Entry.Group(settings)

  private val layout: List[Entry] = List(
    comment("Serenity Editor Configuration"),
    group(_ => List("config.version" -> HoconValue.number(ConfigVersion.Current.value))),
    blank,
    comment("Character animation style: none, quick, smooth, subtle, custom"),
    group(ConfigGroups.characterAnimation),
    blank,
    comment("Syntax highlighting: true, false"),
    field("syntax.highlighting"),
    blank,
    comment("Font configuration"),
    field("font.code.family"),
    field("font.text.family"),
    field("font.ui.family"),
    field("font.code.size"),
    field("font.text.size"),
    field("font.ui.size"),
    field("font.scale.mode"),
    field("font.text_scale"),
    field("font.code.ligatures"),
    field("font.text.ligatures"),
    field("font.ui.ligatures"),
    blank,
    comment("Cursor colour overrides. Leave empty to use the active theme cursor."),
    field("cursor.mode"),
    field("cursor.active.color"),
    field("cursor.inactive.color"),
    comment("Cursor info bar colour overrides. Leave empty to use the active theme's panel colour."),
    field("cursor.info_bar.foreground_color"),
    field("cursor.info_bar.background_color"),
    comment(
      "Comma-separated segment list (title, position, word_count, char_count, reading_time), or off/empty to hide"
    ),
    field("cursor.info_bar.segments"),
    field("cursor.info_bar.placement"),
    blank,
    comment("Interface density: compact, comfortable, spacious"),
    field("interface.density"),
    comment(
      "Window chrome: auto uses themed chrome on Linux; native preserves OS snap/window animations; native-themed " +
        "uses Windows system chrome colours; custom is themed and applies after restart"
    ),
    field("window.chrome"),
    field("window.sitter.enabled"),
    field("window.sitter.action"),
    field("window.sitter.frames"),
    field("window.sitter.active_ticks"),
    field("window.sitter.fast_active_ticks"),
    field("window.sitter.fast_typing_threshold_ms"),
    field("ui.element_gap"),
    field("ui.corner_radius"),
    field("ui.outline_thickness"),
    field("command_runner.visible_rows"),
    field("command_runner.item_gap_rows"),
    field("command_runner.cursor_gap_rows"),
    field("command_runner.show_key_hints"),
    comment("Hold or double-tap a bare modifier to peek the command runner near the cursor"),
    field("command_runner.cursor_peek.enabled"),
    field("command_runner.cursor_peek.modifier"),
    field("command_runner.cursor_peek.tap_window_ms"),
    field("command_runner.cursor_peek.placement"),
    field("render.fps"),
    comment(
      "Damage granularity the renderer honours: rows redraws whole visible lines; cells honours column ranges on"
    ),
    comment("monospaced buffers only, falling back to rows for proportional or ligature-shaped text"),
    field("render.damage_granularity"),
    comment("Cursor info bar background alpha (0.0-1.0), overriding the active theme's own panel alpha for just that"),
    comment("panel. auto/default keeps the theme's alpha."),
    field("display.cursor_info_bar_background_alpha"),
    field("display.word_wrap"),
    field("display.visual_line_navigation"),
    comment("Keep the cursor's line vertically centred (typewriter scrolling), padding past the document's end"),
    field("display.typewriter_scrolling"),
    field("display.line_numbers"),
    field("display.gutter"),
    field("display.word_count"),
    comment("Where document comments are shown: floating, margin"),
    field("display.comments"),
    field("display.pane_headers"),
    field("display.focused_text_body"),
    field("display.contextual_toolbar"),
    field("display.contextual_toolbar_mode"),
    blank,
    comment(
      "UI material and motion presets: solid, clear, frosted, crystal, custom / reduced, subtle, smooth, expressive, " +
        "custom"
    ),
    field("ui.material"),
    comment("Post-processing: off, scanlines, glow, scanlines-glow"),
    field("ui.post_processing"),
    comment("Draw soft shadows behind menus and panels"),
    field("ui.shadows"),
    comment("Background treatment behind panes: solid, transparent, frosted, glass-like"),
    field("ui.background_style"),
    comment("Blur strength behind translucent surfaces (0.0-1.0)"),
    field("ui.blur_radius"),
    group(ConfigGroups.motion),
    blank,
    comment("Markdown rendering mode: source, split-preview, inline-lens"),
    field("document.markdown_view"),
    blank,
    comment("Default mode for new buffers: plain-text, markdown, rich-text"),
    field("document.default_mode"),
    blank,
    comment(
      "App mode: code, prose -- filters which settings are shown by default and gates code-only tooling (LSP, " +
        "project build/run/test/debug)"
    ),
    field("app.mode"),
    comment("Show every setting regardless of the app mode filter above"),
    field("app.show_all_settings"),
    blank,
    field("editor.minimum_pane_width"),
    comment("Lines one mouse-wheel notch scrolls"),
    field("input.wheel_scroll_lines"),
    blank,
    comment("Preferred desktop window size. Leave empty to use the default."),
    field("window.preferred.width"),
    field("window.preferred.height"),
    blank,
    comment("Text area insets as percentages of the central workspace."),
    field("text_area.left.percent"),
    field("text_area.right.percent"),
    field("text_area.top.percent"),
    field("text_area.bottom.percent"),
    field("viewport.width.percent"),
    field("viewport.width.max"),
    field("viewport.height.percent"),
    field("viewport.height.max"),
    blank,
    comment("LSP server overrides"),
    group(config => ConfigGroups.lsp(config.languageToolsConfig.lspUserConfig)),
    blank,
    comment("Spell-checking for prose buffers"),
    field("spellcheck.enabled"),
    field("spellcheck.languages"),
    field("spellcheck.dictionary_paths"),
    field("spellcheck.words"),
    blank,
    comment("Hotkey overrides"),
    group(ConfigGroups.hotkeys),
    blank,
    comment("Focused keymap overrides"),
    group(ConfigGroups.keymaps)
  )
