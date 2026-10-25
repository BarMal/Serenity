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
  * this file: it nests dotted keys into blocks (`editor { cursor { mode = ... } }`), sorts them alphabetically, losing
  * the grouping [[Entry.Comment]] and [[Entry.Blank]] exist to express, and quotes every key containing an underscore
  * (`"info_bar"`). It renders authored comments perfectly well -- `setComments(true)` with `setOriginComments(false)`
  * emits them without the library's own provenance lines -- so comments are not the reason; layout is. This is a file
  * people open and edit.
  */
object ConfigFileFormat:

  enum Entry:
    case Comment(text: String)
    case Blank
    case Field(key: String)
    case Group(settings: (AppConfig, String) => List[(String, HoconValue)])

  /** `osName` picks the platform defaults that hotkeys are compared against, so the file holds only what differs.
    * `omitting` names settings left out, their comment lines kept: the chrome a layout supplies is not worth writing.
    */
  def render(config: AppConfig, osName: String = HotkeyOverrides.runningOs, omitting: Set[String] = Set.empty): String =
    lines(config, osName)
      .filter {
        case Right((key, _)) => !omitting.contains(key)
        case Left(_)         => true
      }
      .map {
        case Left(comment)       => comment
        case Right((key, value)) => s"$key = ${value.rendered}"
      }
      .mkString("", "\n", "\n")

  /** Every setting [[render]] would write, in the order it writes them. */
  def settings(config: AppConfig, osName: String = HotkeyOverrides.runningOs): List[(String, HoconValue)] =
    lines(config, osName).collect { case Right(setting) => setting }

  /** The settings [[render]] would emit that reading the file back would not return, empty when there are none.
    *
    * A key at a path that also has children (`editor.cursor` alongside `editor.cursor.mode`) is not an error to the
    * library -- the later assignment simply replaces the earlier value with an object -- so assembling the settings
    * into a `Config` and counting what survives is what reveals it. A duplicated key shows up the same way.
    */
  def unwritableSettings(config: AppConfig, osName: String = HotkeyOverrides.runningOs): List[String] =
    val settings  = lines(config, osName).collect { case Right((key, value)) => key -> value.config }
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

  private def lines(config: AppConfig, osName: String): List[Either[String, (String, HoconValue)]] =
    layout.flatMap {
      case Entry.Comment(text)   => List(Left(s"# $text"))
      case Entry.Blank           => List(Left(""))
      case Entry.Group(settings) => settings(config, osName).map(Right.apply)
      case Entry.Field(key) => ConfigRegistry.find(key).toList.map(configField => Right(configField.setting(config)))
    }

  private def field(key: String): Entry    = Entry.Field(key)
  private def comment(text: String): Entry = Entry.Comment(text)
  private def blank: Entry                 = Entry.Blank

  private def group(settings: AppConfig => List[(String, HoconValue)]): Entry =
    Entry.Group((config, _) => settings(config))

  private def platformGroup(settings: (AppConfig, String) => List[(String, HoconValue)]): Entry = Entry.Group(settings)

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
    comment("Saving: write a file's unsaved changes to the file itself. off, after-delay, on-focus-change (switching"),
    comment("buffers or leaving the window) or on-window-change (leaving the window)"),
    field("files.auto_save"),
    comment("Milliseconds after the last edit that after-delay waits before saving (at least 100)"),
    field("files.auto_save_delay_ms"),
    blank,
    comment("Editor: what the text area shows"),
    field("editor.syntax_highlighting"),
    field("editor.smart_punctuation"),
    comment("Large files: a file above warn_above_chars opens with a warning; above tooling_off_above_chars (while"),
    comment("optimizations is on) it also goes without language servers and spell check"),
    field("editor.large_file.optimizations"),
    field("editor.large_file.warn_above_chars"),
    field("editor.large_file.tooling_off_above_chars"),
    comment("Indentation: the width of a tab, and whether Tab inserts spaces (true) or a tab character (false)"),
    field("editor.tab_size"),
    field("editor.insert_spaces"),
    field("editor.word_wrap"),
    comment("A scrollbar in each editor pane: a thin overlay in the window, the last column in a terminal"),
    field("editor.scrollbar"),
    comment("The tab strip: auto shows it once two or more files are open, off never does"),
    field("editor.tab_strip"),
    field("editor.visual_line_navigation"),
    comment("Keep the cursor's line vertically centred (typewriter scrolling), padding past the document's end"),
    field("editor.typewriter_scrolling"),
    comment(
      "E-reader-style column layout: as many columns as fit the target width (in cells) plus the gap between " +
        "them. Only takes effect while word wrap is also on"
    ),
    field("editor.column_mode"),
    field("editor.column_target_width"),
    field("editor.column_gap"),
    field("editor.column_count"),
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
    comment("Cursor: blink or steady; colour overrides leave empty to use the active theme cursor"),
    field("editor.cursor.mode"),
    field("editor.cursor.active_color"),
    field("editor.cursor.inactive_color"),
    comment("Milliseconds the caret blinks without input before it holds solid; 0 blinks for as long as focused"),
    field("editor.cursor.blink_timeout_ms"),
    comment("Markdown rendering mode: source, split-preview, live-preview, read"),
    field("editor.markdown_view"),
    comment("Default mode for new buffers: plain-text, markdown, rich-text"),
    field("editor.default_document_mode"),
    comment("Word-count goal for the active document; auto means no goal is set"),
    field("document.word_goal"),
    comment("Name new comments and replies are written under; auto uses the operating system's user name"),
    field("document.comment_author"),
    comment("Multi-line drop caps: render a drop-cap paragraph's first character as a large, multi-line glyph"),
    field("document.drop_caps_enabled"),
    comment("Room between the ink of a drop cap's glyph and the text beside it, in em of the body font (0-2)"),
    field("document.drop_cap_gap"),
    comment("Drop cap colour: text, heading (what headings.colour gives a level 1 heading), accent or #RRGGBB"),
    field("document.drop_cap_colour"),
    comment("Show a chapter note's overview, faded, under an empty chapter when a session starts"),
    field("document.chapter_ghosts"),
    comment("Milliseconds edits must pause before spell check re-analyses the document and the outline re-parses"),
    field("document.analysis_debounce_ms"),
    field("document.outline_debounce_ms"),
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
    comment(
      "Fonts tried, in order, for characters the code, prose or UI font has no glyph for (CJK, Arabic, symbols); " +
        "the system's own fonts follow. Family names, e.g. [\"Noto Sans CJK JP\"]"
    ),
    field("typography.fallback.families"),
    comment("Prose line length in characters (e.g. 66ch) while the workspace is in prose mode, or off"),
    field("typography.prose.measure"),
    comment("How all text adapts to the display: auto, manual (uses the factor below), off"),
    field("typography.scale.mode"),
    field("typography.scale.factor"),
    blank,
    comment("Export and print typography (separate from the screen): preset smf-courier; paper letter or a4;"),
    comment("font size, line spacing (a multiple), margin and first-line indent in points, or auto for the preset's"),
    field("export.typography.preset"),
    field("export.typography.paper"),
    field("export.typography.font_size"),
    field("export.typography.line_spacing"),
    field("export.typography.margin"),
    field("export.typography.first_line_indent"),
    blank,
    comment("Headings: each level's size is a multiple of the body size, and every setting is auto (the default look)"),
    comment("unless set. Bold and italic take auto, true or false; align takes auto, left, centre, right or justify."),
    comment("Colour takes auto (follow headings.colour), text, heading (the theme's heading colour) or #RRGGBB."),
    comment(
      "A paragraph's own alignment still wins. space_before and space_after are the room above the first line and"
    ),
    comment(
      "below the last line of a heading, in em of its own font size (0-8, auto is none); a terminal rounds them to"
    ),
    comment("whole rows. No space is added above the first line of a document or a page."),
    comment(
      "Underline takes auto, true or false. A terminal has no sizes, so auto there climbs a ladder: level 1 bold and"
    ),
    comment(
      "underlined, 2 bold, 3 bold italic, and levels 4 to 6 bold in the accent, string and muted colours. Set a key"
    ),
    comment(
      "to take a level off it. Colour also takes accent, secondary or muted. Rule takes auto, true or false; a terminal with"
    ),
    comment("it on draws a line of box-drawing cells in the row below the heading, which space_after auto reserves."),
    field("headings.h1.size"),
    field("headings.h1.bold"),
    field("headings.h1.italic"),
    field("headings.h1.family"),
    field("headings.h1.colour"),
    field("headings.h1.align"),
    field("headings.h1.space_before"),
    field("headings.h1.space_after"),
    field("headings.h1.underline"),
    field("headings.h1.rule"),
    field("headings.h2.size"),
    field("headings.h2.bold"),
    field("headings.h2.italic"),
    field("headings.h2.family"),
    field("headings.h2.colour"),
    field("headings.h2.align"),
    field("headings.h2.space_before"),
    field("headings.h2.space_after"),
    field("headings.h2.underline"),
    field("headings.h2.rule"),
    field("headings.h3.size"),
    field("headings.h3.bold"),
    field("headings.h3.italic"),
    field("headings.h3.family"),
    field("headings.h3.colour"),
    field("headings.h3.align"),
    field("headings.h3.space_before"),
    field("headings.h3.space_after"),
    field("headings.h3.underline"),
    field("headings.h3.rule"),
    field("headings.h4.size"),
    field("headings.h4.bold"),
    field("headings.h4.italic"),
    field("headings.h4.family"),
    field("headings.h4.colour"),
    field("headings.h4.align"),
    field("headings.h4.space_before"),
    field("headings.h4.space_after"),
    field("headings.h4.underline"),
    field("headings.h4.rule"),
    field("headings.h5.size"),
    field("headings.h5.bold"),
    field("headings.h5.italic"),
    field("headings.h5.family"),
    field("headings.h5.colour"),
    field("headings.h5.align"),
    field("headings.h5.space_before"),
    field("headings.h5.space_after"),
    field("headings.h5.underline"),
    field("headings.h5.rule"),
    field("headings.h6.size"),
    field("headings.h6.bold"),
    field("headings.h6.italic"),
    field("headings.h6.family"),
    field("headings.h6.colour"),
    field("headings.h6.align"),
    field("headings.h6.space_before"),
    field("headings.h6.space_after"),
    field("headings.h6.underline"),
    field("headings.h6.rule"),
    comment(
      "headings.colour is auto (rich-text headings in the text colour, Markdown headings in the heading colour),"
    ),
    comment("text, heading, accent, secondary or muted, for both kinds of document."),
    field("headings.colour"),
    comment("Above a body of damping_threshold points (before any device scale) heading sizes grow more slowly"),
    field("headings.damping"),
    field("headings.damping_threshold"),
    blank,
    comment("Chapter openings: the first paragraph after each chapter title (and, per scene_break, after a scene"),
    comment("break) can be set off. enabled is off by default. letter is drop, raised or hybrid (a raised letter"),
    comment("that sinks hybrid_sink_lines, 2-6, lines); raised_row is grow or clamp; small_caps sets the first line"),
    comment("in small capitals. epigraph is auto, never or marker (only paragraphs carrying epigraph_marker); a"),
    comment("scene_break is none, small-caps or full; hand_drop_cap, for an opening that already has a hand-made"),
    comment("drop cap, is hand, hand-small-caps or opening. A document can override every one of these."),
    field("openings.enabled"),
    field("openings.letter"),
    field("openings.raised_row"),
    field("openings.hybrid_sink_lines"),
    field("openings.small_caps"),
    field("openings.epigraph"),
    field("openings.epigraph_marker"),
    field("openings.scene_break"),
    field("openings.hand_drop_cap"),
    blank,
    comment("Look: interface density (compact, comfortable, spacious)"),
    field("ui.density"),
    comment("How strongly a misspelled-word/diagnostic highlight's severity colour shows through (0.0-1.0)"),
    field("ui.diagnostic_highlight_blend_weight"),
    comment(
      "File dialogs for open and save: auto uses the system dialog unless GTK's schemas are missing (which would"
    ),
    comment(
      "crash the app), then Swing's own; native always uses the system dialog; swing never does. Needs a restart"
    ),
    field("ui.file_dialog"),
    field("ui.element_gap"),
    field("ui.outline_thickness"),
    comment("Layout: minimal starts with line numbers, pane headers, the status bar, the tab strip and the scrollbar"),
    comment("all off, conventional with all of them on. It only chooses what those start as: any of their own keys"),
    comment("(editor.line_numbers, editor.pane_headers, status.placement, editor.tab_strip, editor.scrollbar) you"),
    comment("set anywhere in this file still wins"),
    field("ui.layout"),
    comment("Menu bar in the desktop window: hidden (every command stays in the command runner), alt (shown while Alt"),
    comment("is tapped on its own) or visible (always shown). Applies after restart"),
    field("ui.menu_bar"),
    comment("Hold a chord to show hidden chrome for as long as it is held: the modifiers below with the letter of the"),
    comment(
      "element (line numbers, status bar, tab strip, scrollbar, pane headers). Press the lock key while holding it"
    ),
    comment("to keep the element shown; press the chord again to hide it. A letter set to off has no chord. The"),
    comment("terminal cannot see a key released, so there the chord shows and locks, and pressing it again hides"),
    field("ui.summon.modifiers"),
    field("ui.summon.lock"),
    field("ui.summon.line_numbers"),
    field("ui.summon.status_bar"),
    field("ui.summon.tab_strip"),
    field("ui.summon.scrollbar"),
    field("ui.summon.pane_headers"),
    comment("Where Escape in a focused panel returns focus, per app mode: editor (the active pane) or previous"),
    comment("(whatever had focus before the panel)"),
    field("ui.panel.escape_returns_to.code"),
    field("ui.panel.escape_returns_to.prose"),
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
    comment("Renderer for the window: auto uses Skia's GPU layer where it loads, falling back to Skia on the CPU and"),
    comment("then to Java2D; skia or java2d forces one."),
    comment("SERENITY_RENDER_BACKEND overrides this. Needs a restart."),
    field("ui.render.backend"),
    comment("On a Wayland session on native Wayland: skia-raster, the default, paints Skia on the CPU; java2d keeps"),
    comment("Java2D; xwayland switches to the X11 toolkit so Skia's GPU layer can run."),
    comment("SERENITY_WAYLAND_RENDER overrides this. Needs a restart."),
    field("ui.render.wayland"),
    comment(
      "Make Skia's GPU layer wait for the display's vertical blank before it presents a frame. Off by default: it"
    ),
    comment(
      "adds a few milliseconds of latency, and compositors (Hyprland, GNOME, Windows, macOS) do not tear without it."
    ),
    comment("Turn it on if you see tearing, for example on X11 without a compositor. SKIKO_VSYNC_ENABLED or"),
    comment("-Dskiko.vsync.enabled beat this. Needs a restart."),
    field("ui.render.skia_vsync"),
    comment("Per-cache capacity for the renderer's bounded frame-state caches (issue #1433). Default 64 suits a"),
    comment("single window; raise it for a session with many concurrently open surfaces."),
    field("ui.render.cache_capacity"),
    comment("Cache modal/panel layers between frames instead of repainting them (each holds a full-window image)"),
    field("ui.render.layer_cache"),
    comment("Log a [FRAME] timing summary (input, render and paint latency) to the app log every 5 seconds"),
    field("ui.render.frame_timing"),
    comment("Log a [LATENCY] line per keystroke (each stage from key event to paint) and a summary every 5 seconds"),
    field("ui.render.latency_trace"),
    comment("After the first frame, briefly exercise typing and drawing off-screen so early keystrokes are not slow"),
    field("startup.warm_up"),
    comment("Return unused heap to the operating system sooner (smaller idle memory, a little more collector work)."),
    comment(
      "Applies at startup, so a change needs a restart. Under G1 it sets MinHeapFreeRatio=10, MaxHeapFreeRatio=30"
    ),
    comment("and G1PeriodicGCInterval=30000; false leaves the JVM's launch values alone."),
    field("performance.memory.compact"),
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
    comment("Theme: follow the operating system's light, dark or high-contrast setting (off by default). When on, the"),
    comment("theme is re-checked at startup and on window focus; picking a theme by hand turns it off"),
    field("theme.follow_system"),
    field("theme.light"),
    field("theme.dark"),
    field("theme.high_contrast"),
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
    comment("Hotkey overrides: only the bindings you changed are listed, as hotkey.<action> = [\"ctrl+alt+k\"];"),
    comment("anything not listed keeps its default, and [] unbinds an action"),
    platformGroup((config, osName) => ConfigGroups.hotkeys(config, osName)),
    blank,
    comment("Keys for any palette command, by its id: hotkey.command.<id> = [\"ctrl+alt+k\"]; [] unbinds it"),
    platformGroup((config, osName) => ConfigGroups.commandHotkeys(config, osName)),
    blank,
    comment("Focused keymap overrides"),
    group(ConfigGroups.keymaps)
  )
