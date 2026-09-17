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
  * The comments and ordering are authored rather than generated because the config library's renderer would restructure
  * this file: it nests dotted keys into blocks (`character { animation = ... }`), sorts them alphabetically, losing the
  * grouping [[Entry.Comment]] and [[Entry.Blank]] exist to express, and quotes every key containing an underscore
  * (`"info_bar"`). It renders authored comments perfectly well -- `setComments(true)` with `setOriginComments(false)`
  * emits them without the library's own provenance lines -- so comments are not the reason; layout is. This is a file
  * people open and edit.
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
    comment("Workspace: code or prose mode (filters which settings are shown and gates code-only tooling: LSP,"),
    comment("project build/run/test/debug), and pane layout"),
    field("workspace.mode"),
    comment("Show every setting regardless of the mode filter above"),
    field("workspace.show_all_settings"),
    field("workspace.minimum_pane_width"),
    blank,
    comment("Editor: what the text area shows"),
    field("editor.syntax_highlighting"),
    field("editor.word_wrap"),
    field("editor.visual_line_navigation"),
    comment("Keep the cursor's line vertically centred (typewriter scrolling), padding past the document's end"),
    field("editor.typewriter_scrolling"),
    field("editor.line_numbers"),
    comment(
      "Line-number placement (left, right, both) and cell spacing: margin from the panel edge to the counter, " +
        "padding between the counter and the content"
    ),
    field("editor.line_number_side"),
    field("editor.line_number_margin_left"),
    field("editor.line_number_margin_right"),
    field("editor.line_number_padding"),
    comment("Where document comments are shown: floating, margin"),
    field("editor.comments"),
    field("editor.pane_headers"),
    field("editor.focused_text_body"),
    field("editor.contextual_toolbar"),
    field("editor.contextual_toolbar_mode"),
    comment("Lines one mouse-wheel notch scrolls"),
    field("editor.wheel_scroll_lines"),
    comment("Cursor: blink or breathe; colour overrides leave empty to use the active theme cursor"),
    field("editor.cursor.mode"),
    field("editor.cursor.active_color"),
    field("editor.cursor.inactive_color"),
    comment("Markdown rendering mode: source, split-preview, inline-lens"),
    field("editor.markdown_view"),
    comment("Default mode for new buffers: plain-text, markdown, rich-text"),
    field("editor.default_document_mode"),
    comment("Text area insets as percentages of the central workspace"),
    field("editor.text_area.left"),
    field("editor.text_area.right"),
    field("editor.text_area.top"),
    field("editor.text_area.bottom"),
    blank,
    comment("Status line: comma-separated segments (position, title, language, mode, word_count, char_count,"),
    comment(
      "reading_time) or off; placement pinned (a row under the workspace), floating (a quiet row that follows the"
    ),
    comment("caret) or off. Colour and alpha overrides leave empty/auto to use the active theme's panel colours."),
    field("status.segments"),
    field("status.placement"),
    field("status.foreground_color"),
    field("status.background_color"),
    field("status.background_alpha"),
    blank,
    comment("Typography"),
    field("typography.prose.family"),
    field("typography.code.family"),
    field("typography.ui.family"),
    field("typography.prose.size"),
    field("typography.code.size"),
    field("typography.ui.size"),
    field("typography.prose.ligatures"),
    field("typography.code.ligatures"),
    field("typography.ui.ligatures"),
    comment("How all text adapts to the display: auto, manual (uses the factor below), off"),
    field("typography.scale.mode"),
    field("typography.scale.factor"),
    blank,
    comment("Look: interface density (compact, comfortable, spacious), material and background"),
    field("ui.density"),
    comment(
      "UI material: solid, clear, frosted, crystal, custom"
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
    comment("How strongly a misspelled-word/diagnostic highlight's severity colour shows through (0.0-1.0)"),
    field("ui.diagnostic_highlight_blend_weight"),
    field("ui.element_gap"),
    field("ui.corner_radius"),
    field("ui.outline_thickness"),
    comment("Command runner rows and spacing; auto follows the interface density and the window height"),
    field("ui.command_runner.visible_rows"),
    field("ui.command_runner.item_gap_rows"),
    field("ui.command_runner.cursor_gap_rows"),
    field("ui.command_runner.show_key_hints"),
    comment("Hold or double-tap a bare modifier to peek the command runner near the cursor"),
    field("ui.command_runner.cursor_peek.enabled"),
    field("ui.command_runner.cursor_peek.modifier"),
    field("ui.command_runner.cursor_peek.tap_window_ms"),
    field("ui.command_runner.cursor_peek.placement"),
    field("ui.render.fps"),
    comment(
      "Damage granularity the renderer honours: rows redraws whole visible lines; cells honours column ranges on"
    ),
    comment("monospaced buffers only, falling back to rows for proportional or ligature-shaped text"),
    field("ui.render.damage_granularity"),
    comment("Per-cache capacity for the renderer's bounded frame-state caches (issue #1433). Default 64 suits a"),
    comment("single window; raise it for a session with many concurrently open surfaces."),
    field("ui.render.cache_capacity"),
    comment("Visual flair tier for purely decorative extras (companion sprite, background blur): full, reduced, off"),
    field("ui.visual_flair"),
    comment("Companion sprite: a small pixel-art character idling in a pinned pane, reacting to typing (issue #934)"),
    field("ui.companion_sprite.enabled"),
    field("ui.companion_sprite.character"),
    field("ui.companion_sprite.position"),
    field("ui.companion_sprite.size"),
    field("ui.companion_sprite.typing_cycle"),
    field("ui.companion_sprite.typing_active_ticks"),
    field("ui.companion_sprite.typing_fast_active_ticks"),
    field("ui.companion_sprite.typing_fast_threshold_ms"),
    blank,
    comment("Motion: preset (reduced, subtle, smooth, expressive, custom), accessibility, and per-family overrides"),
    group(ConfigGroups.motion),
    comment("Character animation style: none, quick, smooth, subtle, custom"),
    group(ConfigGroups.characterAnimation),
    blank,
    comment(
      "Window chrome: auto uses themed chrome on Linux; native preserves OS snap/window animations; native-themed " +
        "uses Windows system chrome colours; custom is themed and applies after restart"
    ),
    field("window.chrome"),
    comment("Preferred desktop window size. Leave empty to use the default."),
    field("window.preferred.width"),
    field("window.preferred.height"),
    comment("Editor viewport as a share of the window, with optional caps in cells"),
    field("window.viewport.width_percent"),
    field("window.viewport.width_max"),
    field("window.viewport.height_percent"),
    field("window.viewport.height_max"),
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
