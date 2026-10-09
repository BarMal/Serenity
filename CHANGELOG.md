# Changelog

## [Unreleased]

### 2026-10-09

- Added recent folders to Open Recent: every folder opened as the project root, by Open Folder, on the command line or handed over by a later launch, is remembered (most recent first, once however its path is spelt, twenty at most) in an Open Recent Folder submenu beside Open Recent and on the start page after the recent files, and choosing one opens it in the Explorer as Open Folder does. They are saved with the session (schema version 6; an older session loads with none), the start page leaves out a folder that has gone, and Clear Recent now forgets the folders as well as the files.

### 2026-10-08

- Made the start page and Cmd+O on macOS offer one "Open..." that takes a file or a folder, through the native open panel with both choices on, as VS Code, Zed and Pulsar do: a folder opens in the Explorer and leaves the start page, a file opens in a buffer, and cancelling changes nothing. Windows, Linux and the terminal keep Open file and Open folder, the palette lists Open File..., Open Folder... and, on macOS, Open..., and if the macOS panel cannot be shown the dialog failure notice says why. The macOS File menu is not built yet, so its Open... entry is ready but not visible.
- Stopped Backspace, Delete and typing doing nothing after a selection collapsed onto its own anchor (Shift+Right then Shift+Left, Shift+Left at the start of the document, Shift+End at the end of a line): a selection that selects nothing is now no selection, so the edit lands, dirties the buffer and is undoable.
- Bound Cmd+Backspace and Cmd+Delete to delete to the start and the end of the line, and Option+Delete to delete the word forward, each deleting the selection instead when there is one and working at every cursor; Shift+Backspace and Shift+Delete now act as Backspace and Delete instead of being dropped.
- Split "Open file or folder" on the start page into Open file and Open folder, added an "Open Folder..." command to the palette and File menu, and made the folder picker work: a folder chosen in the native dialog opens in the Explorer and leaves the start page, a folder passed to Open File now says it is a folder, and a native dialog that cannot be shown reports why instead of only logging it.
- In the terminal, where there is no native dialog, Open folder (start page and palette) now opens a dedicated Open Folder form instead of the generic Open form: it lists folders only, `Enter` browses into the folder named in the Path, `Tab` descends into the highlighted one, `Ctrl+R` opens the shown folder in the Explorer, and `Esc` cancels.
- Put away the code-only panels, such as the project output, whenever the mode changes to prose, not only for the mode command: a config file edit, a workflow preset and Reset Settings now do the same, in the same write as the mode change.
- Made leaving code mode release the language servers: open documents are closed, servers are shut down and exited, and their diagnostics, semantic tokens and progress are cleared, and entering code mode opens every document that has a language and a file again, whether the mode changes by command, workflow preset or an outside edit of the config file.
- Asked before leaving code mode while a build, test or run task is running, offering to stay or stop the task and switch; a mode change nobody was asked about, such as a config file edit, stops the task and says so.

### 2026-10-06

- Turned spell check on by default with British English (`en-GB`) as the language, bundled the LibreOffice en_GB Hunspell dictionary so it works with nothing installed, and read language codes in any casing (#2070, #2060).
- Drew a misspelling as a red squiggle in the theme's error colour instead of the shared diagnostic background, which read as a warning or search highlight; a terminal underlines the word in the error colour (#2070).
- Held dictionaries as stems and affixes rather than expanding every surface form at load (12.8 MB of heap instead of 38.6 MB), and checked prose word by word, skipping code, HTML tags, URLs, email addresses, link targets and front matter, so correct British prose is no longer flagged (#2070).
- Offered ranked corrections for a misspelling from a right-click menu, with Add to Dictionary, Ignore Once and Ignore All, plus Show Spelling Suggestions, Ignore Misspelling Once and Ignore Misspelling Everywhere in the command palette; choosing a correction is one undoable edit (#2070).
- Repainted diagnostics that arrive without moving a line, such as spell check finishing or being switched off, which previously stayed stale until the layout changed, and under the command runner until it closed (#2070).
- Named every configured language that has no dictionary in the missing-dictionary notice, with a `hunspell-<lang>` package suggestion for each, instead of only noticing when no language resolved at all (#2083).
- Kept every valid setting when one config value is invalid: the bad value falls back to its default and is listed with its key and reason on the start page, an unparseable file is left untouched with saves refused, and a hotkey clash costs only the clashing binding rather than every hotkey (#2071).
- Saved the config by patching only the lines that changed, so a hand-written file keeps its comments, ordering, key spellings, line endings and unrecognised keys, and made a timestamped backup before replacing an invalid value or a newer-version file (#2071).
- Added export of a manuscript as DOCX or EPUB 3 from an "Export Manuscript..." palette command (also `export-manuscript-docx` and `export-manuscript-epub`), compiled from an optional `manuscript.conf` with chapter detection, title page, dedication, end marker, replacements and Modern and Classic presets (#2064).
- Localised the EPUB navigation headings, title page, dedication and untitled-chapter titles for English, French, German, Spanish, Italian, Portuguese and Dutch, with `labels.*` overrides in `manuscript.conf` (#2077).
- Kept raw HTML in Markdown manuscript sources as meaning rather than dropping it: `<b>`, `<i>` and `<u>` become marks, `<br>` a line break and `<hr>` a scene break, and any other tag is stripped with its text kept (#2077).
- Counted words by Unicode word boundaries (UAX #29), with ideographs and kana counted one per character and hyphenated compounds counted once, and characters by grapheme cluster, kept incrementally in the rope so edits stay cheap (#2053).
- Stopped Ctrl+Left and Ctrl+Right at dictionary words in spaceless CJK text and treated an emoji sequence or flag as one stop, and added sub-word motion through camelCase, PascalCase and snake_case on Ctrl+Alt+Left and Ctrl+Alt+Right, with Shift-extend variants (#2053).
- Bound macOS Redo to Cmd+Shift+Z, keeping Cmd+Y as a secondary, and added Ctrl+Shift+Z beside Ctrl+Y on Windows and Linux (#2080).
- Searched Find from the caret and wrapped, with Alt+C, Alt+W and Alt+R toggling match case, whole word and regular expressions, an invalid expression shown as an error, matches capped at "1000+", and visible and current matches highlighted in the text; F3 and Shift+F3 no longer land on stale offsets (#2058).
- Added safe mode (`--safe-mode`, or "Restart in Safe Mode" in the palette), which starts with default settings, a scratch session, and no language servers, spell check or project tasks, and never writes config; `--reset-config` and `--reset-session` and a "Reset Settings" command move the files aside to timestamped backups (#2047).
- Started in safe mode automatically after two unfinished starts in a row, with a notice and a prompt offering "Restart normally" (#2047).
- Forwarded a second launch's file arguments to the running instance and exited, and fell back to a clearly flagged temporary session when the running instance cannot be reached; retried the lock on Windows, where a killed process's lock is released asynchronously (#2037, #2057).
- Moved a corrupt or newer-schema session aside to a timestamped backup before anything can save over it, salvaged its unsaved text to plain files, and showed a "Session not restored" prompt (#2037).
- Answered language-server requests (configuration, capability registration, progress, `showMessageRequest`, `applyEdit`), restarted a crashed server with backoff and reopened its documents, released a server with no open document after 30 seconds, and showed server progress in the status line (#2048).
- Drained language-server stderr into a size-capped `~/.serenity/logs/lsp-<language>.log`, since a chatty server filled the pipe and blocked every request, and bounded connection release so a hung server cannot hang shutdown or restart (#2066, #2072).
- Dropped diagnostics and semantic tokens a server publishes for documents with no open buffer, and forgot the undo history, chapter ghost cache and language-server data of closed buffers (#2075, #2063).
- Replaced Swing's `RTFEditorKit` with a native RTF reader and writer handling Unicode escapes, code pages, headings, tables and pictures, with malformed input reported instead of thrown (#2052).
- Measured slanted font faces through their layout advance rather than the sum of their glyph advances (#2045).
- Listed watched directories ourselves on macOS, where the JDK's watcher re-scans only every ten seconds, so external edits surface promptly, and stopped watching files while the window is unfocused, rechecking everything once on focus (#2044, #2074).
- Skipped session writes when nothing changed, encoded the config only when it changed, and made the edit-idle session save cost what changed rather than everything that is open (#2076, #2061).
- Bounded the Markdown preview image caches by bytes (96 MiB) rather than entry count, whose worst case exceeded 1 GB, and cached decoded local images instead of re-reading them on every render (#2067).
- Stopped silent project tasks scheduling timers and gated frame-timing recording behind its report (#2059).
- Licensed Serenity under GPL-3.0-or-later and shipped the licence, the Monaspace font's OFL text and a generated `THIRD-PARTY-NOTICES.md` in the jar and all three app images, opened read-only by a new "Show Licence and Notices" palette command; CI fails when a runtime dependency has no notice or the file is stale (#2041).

### 2026-10-05

- Showed failed saves, save-as, session saves, opens and reloads as non-modal corner notices naming the file and the cause; errors stay until dismissed, other notices dismiss themselves, and Escape closes them (#2035).
- Scrolled the explorer, outline, comments and diagnostics panels independently of their selection, so the wheel scrolls without moving the highlight and PageUp, PageDown, Home and End work in every list panel (#2036).
- Pasted a whole-line copy, made with Copy or Cut and no selection, as a line above the caret line, normalised line endings in clipboard text, and added a "Paste from History" picker over a bounded clipboard history (#2034).
- Made the Writing preset enable smart punctuation, spell check, typewriter scrolling, paragraph focus and a words and goal status line, hold the text column at `typography.prose.measure` (66ch) in prose mode, and keep the user's theme instead of forcing dark (#2032).
- Handled decade apostrophes in smart punctuation and made Markdown formatting toggles aware of code (#2032).
- Centred the cursor on the painted wrap in rich-text buffers, so row counts follow per-run fonts and drop caps (#2040).
- Checked a file's revision by stat first and hashed at most once per save: on a 10 MiB file a save reads 36 KB instead of 10 MB and focus-in reads 25 bytes (#2038).
- Wrote sessions in a single write, retried a refused rename, and watched files with the platform's own watch service, debounced and without idle wakeups (#2038).
- Offered to recover unsaved session edits that differ from the file on startup, keeping the recovered text by default or opening the file from disk (#2011).
- Added a Reopen with Encoding command, for files whose encoding was detected wrongly (#2011).
- Advanced a buffer's content version on every edit, so a debounced outline parse of earlier text is no longer accepted after newer typing (#2011).
- Repainted a few disjoint rects instead of one rect from the caret row to the status row (77 to 98 percent of the window), let pane headers and status chrome join the bounded repaint, and translated damage into screen rows rather than whole wrapped paragraphs (#2033, #2013).

### 2026-10-04

- Protected user writing on save: fsynced the temporary file and directory, wrote through symlinks and hardlinks instead of replacing them, and decoded non-UTF-8 text losslessly so saving writes back the original bytes and BOM (#1969).
- Stopped the comment lens losing drafts: source-code comments stay read-only, an emptied draft deletes the comment rather than saving the word "Comment", edits are grapheme-aware, and duplicate comments resolve by index (#1969).
- Made smart punctuation prose-only, so `--` is an en dash and `---` an em dash, code buffers and Markdown code are skipped, and each substitution is its own undo step (#1970).
- Reached whole notches with high-resolution trackpads, whose wheel gestures were dropped because the integer rotation they report is 0 (#1970).
- Edited the Find query as a real text field, with a caret, Delete, word deletes, paste and grapheme-safe Backspace, instead of appending and trimming a string (#1976).
- Let global hotkeys run any command by id (`hotkey.command.<id>`), through the same availability checks as the palette (#1973).
- Let peeks (hover, definition, references and the read-only comment lens) show without taking focus, closing on Escape, and passed any other key that closes one through to the editor instead of swallowing it (#1971).
- Kept gutter line numbers clear of the divider by right-aligning them in pixels, and pinned the GUI caret to the cursor (#1981).
- Kept the cursor centred in long wrapped paragraphs by no longer clamping the row offset, and re-placed it on resize (#1982).
- Centred a typing burst's run of keys once rather than once per key, so a fast batch no longer drains slower than it queues (#1988).
- Re-wrapped an edited paragraph incrementally, equal to a cold wrap, and followed an edit between two ropes in O(depth + changed region) (#1972, #1968).

### 2026-10-03

- Removed all animations, the animation ticker and motion configuration, which takes out the panel, caret glide, selection and column-sweep motion added from 2026-09-18 to 2026-09-19; config files and sessions that still name the removed settings load, with each key reported once as removed and ignored (#1815, #1807).
- Removed window translucency, rounded window corners and the Transparent theme (#1814).
- Removed the companion sprite pane added on 2026-09-06, its panel and its `ui.companion_sprite.*` and `ui.visual_flair` settings (#1813).
- Removed panel shadows, sheen, rounded corners, glass and blur: panels now draw as opaque `theme.panel.background` with square corners and a plain border at `ui.outline_thickness` (#1811).
- Removed the scanline and glow post-processing pass and its `ui.post_processing` setting (#1807).
- Fixed a GUI layer-cache leak that pinned another full-window image every frame, about 400 MB at 1500x1000, and kept the garbage collector thrashing (#1799).
- Cut the cost of typing in word-wrapped prose by caching wrapped lines shared between state updates and rendering, measuring each wrapped paragraph once instead of every row, indexing visual rows per buffer, and drawing lines from measured runs (#1804, #1819, #1806, #1817).
- Presented only the repaint clip and filled carets instead of overlaying a full-window image, paced frames by deadline, and applied queued input in batches (#1803, #1802, #1820).
- Stopped docked panels adding about 60 ms to every 2x frame and cached panel layers repainting on unrelated edits (#1805).
- Added AppCDS launch flags to the desktop packages, a startup warm-up after the first frame, and native Wayland toolkit selection (#1818).

### 2026-10-02

- Kept placeholders anchored as edits land around them, instead of drifting onto unrelated text (#1776).
- Added chapter notes: hidden buffers, note keys, a ghost outline and a following notes pane (#1789).
- Stacked peeks above the comment lens rather than replacing it, returning focus to the lens when a peek is dismissed (#1792).
- Added a per-mode setting for where Escape from a panel returns focus, the editor's active pane (default) or whatever had focus before, as `ui.panel.escape_returns_to.code` and `.prose` (#1791).
- Stepped Ctrl+Tab and Ctrl+Shift+Tab from the buffer in the active editor pane when a panel or modeless modal has focus, rather than jumping to the first buffer (#1790).
- Added Home, End, PageUp and PageDown to modals, and Tab to the contextual toolbar (#1788).
- Kept modals focused over the command runner, and dismissed a modeless modal on a click outside it (#1787).
- Restored focus through history when workflows, close prompts and peeks end (#1786).
- Grouped Settings > Keys by where each binding applies: Global, Editor, Command Runner, Dialog, Panel and Peek (#1785).
- Removed the panel pin selections left behind by the old per-panel Settings rows (#1794).

### 2026-10-01

- Added a fuzzy "Go to File" finder on Ctrl+E (Cmd+E on macOS) over the files under the docked explorer's root, skipping hidden directories and build output and capped at 20,000 files (#1783).
- Added a generic list picker with a query, filtering, previews and a dismiss command, and moved the session pickers, the theme chooser (live preview, Escape restores) and file search, now "Search in Open Files", onto it (#1768, #1779, #1781, #1782).
- Moved focus between panes and panels with Alt+Arrow, rebindable like any hotkey (#1767).
- Navigated inside every panel from the keyboard: Outline, Comments and Diagnostics answer Up, Down, Home, End, PageUp and PageDown and open the highlighted row with Enter, and the explorer gains Home, End, PageUp and PageDown (#1775).
- Arranged panels from one list in the Panels settings, ordered by edge, instead of cycling edges per panel; Alt+Up and Alt+Down move a panel past its neighbour or into the next edge (#1774).
- Generated per-panel commands from the panel registry: `toggle-<panel>-panel`, `focus-<panel>-panel` and a single `toggle-maximise-panel`, and registered project output and the companion as panels (#1765, #1764, #1763).
- Asked before a plain Save drops a buffer's formatting, offering Save As a rich document, Save without formatting, or Cancel (#1773).
- Refused formatting in code files with a notice, asked first in plain-text files, and added a Convert to Rich Text command that Markdown files refuse (#1762).
- Formatted Markdown files by editing their source: bold, italic and underline toggle `**`, `*` and `<u>` around the selection and headings set the `#` prefix, as one undoable edit (#1766).
- Asked "save changes before closing?" through the generic Confirm prompt (#1771).
- Greyed out project commands with the reason "No project detected." when no supported project encloses the file, instead of hiding them (#1761).
- Hid GUI-only settings in the terminal unless Show All Settings is on, each led by "Inert in TUI mode" when shown (#1760).
- Drew rounded box-drawing frames around floating surfaces and panels in the terminal, and showed the terminal's own backdrop through translucent panels (#1772, #1780).
- Painted a column-mode caret in the one column that owns it, and wrapped column-mode viewport anchoring at the rendered column width so page flips no longer arrive late (#1778, #1777).
- Added commands typed by family (Core, Code, Prose) and frontend support (both, or GUI only), so the palette and settings scope what they offer by app mode and frontend (#1756).
- Listed explorer directories from one place, including a restored session's explorer, which was never listed; panels now show loading, error and empty states, a failed listing is reported rather than only logged, and a directory is re-listed after a change made outside the editor (#1755).
- Kept the Outline, Comments, Diagnostics and Markdown preview panels in step with the active document, where they had been snapshots taken at pin time that went stale on edits and tab switches, and Diagnostics was always empty (#1754).
- Kept docked panels when restoring a session, which previously dropped them and fell back to the pre-restore state (#1753).
- Added a shared set of widget interaction models and drove the context menu from the keyboard with them (#1757).

### 2026-09-30

- Added count-driven columns for the e-reader layout, with live stepper settings (`columnCount` unset means Auto, as before) (#1752).
- Wrapped vertical navigation at the column width in multi-column layout, where Up and Down had wrapped at the full editor width and moved the cursor incorrectly (#1750).

### 2026-09-29

- Added multi-line drop cap paragraphs, rendering a paragraph's first character as a large glyph spanning several lines, with a `paragraph-drop-cap` command (#1749).

### 2026-09-28

- Added word goals: a target word count for the active document, shown as progress in the status line, for example "342 / 1000 words (34%)" (#1748).
- Added darlings: cut a passage to a parking area instead of deleting it, and restore it later (#1747).
- Added placeholders, a to-do marker at a buffer position carrying a short sticky note (#1746).
- Added live chapter-heading renumbering for Markdown prose: a heading shaped like "Chapter <number>", optionally followed by ": Title", keeps its number in sequence as chapters are added, removed or reordered (#1745).
- Added smart punctuation autoformatting while typing (`editor.smart_punctuation`, default off): straight quotes become curly quotes, `--` an em dash and `...` an ellipsis (#1744).

### 2026-09-27

- Stopped the spell checker stat-ing every candidate dictionary file on each keystroke, caching dictionary fingerprints and invalidating them through the file watcher (#1736).
- Stored rich-text documents as a balanced tree keyed by paragraph boundaries, so a range edit rebuilds only the paragraphs it changes instead of the whole list (#1735).

### 2026-09-25

- Saved command runner recents with each session, so commands used in one session no longer leak into another (#1720).
- Kept a buffer open when its close-save fails, where choosing Save in the close prompt closed it whatever the outcome and lost the edits on a failed write or an external-change conflict (#1718).

### 2026-09-24

- Ran file saves, save-as, force-save, reloads and opens on a per-file lane with versioned results, and fixed the save, open and restore data-loss bugs it exposed (#1711).
- Kept theme chooser, theme creator and file search from dropping out of the command palette's recents when run from it (#1715).
- Closed background tabs by id from the close affordance, with mouse hit-testing made a pure function of the event and state (#1703).

### 2026-09-23

- Failed fast when the terminal shell cannot build a real system terminal, instead of silently falling back to an unusable dumb terminal that left a blank screen and echoed keystrokes as control characters, as happened on Termux with OpenJDK 25 (#1668).

### 2026-09-21

- Added LSP Find References and Rename Symbol (#1620).
- Added a keyboard equivalent for tab drag-to-reorder, bound to Ctrl+Shift+PageUp and Ctrl+Shift+PageDown (#1618).
- Added "Save Session As...", "Open Session..." and "Rename Session..." to the command palette, which had no route to the existing named-session support (#1622).
- Exposed the tab bar to the accessibility tree (#1619).
- Painted a close glyph on each tab at its clickable close region, and fixed tab drag hit-testing and a state validation bypass found in a codebase audit (#1613).
- Preserved a file's line endings across an edit and save, so a buffer reproduces the file it was loaded from (#1616).
- Added a typographic spacing scale for UI chrome, gave GUI chrome spacing a surface-aware default so content no longer sits flush against every edge, and insetted overlay and docked-panel glyphs from their borders (#1614, #1621, #1615, #1617).
- Added a preset diff-toggle review surface with a reusable checkbox row for settings, and made the command-runner cursor gap and settings rows surface-aware (#1629, #1630, #1628).

### 2026-09-20

- Added tabs: click to switch buffer, a close affordance per tab, drag to reorder, overflow handling and a new-tab affordance (#1601, #1602, #1608, #1607, #1609).
- Rendered N columns side by side for the e-reader layout, with text flowing from column to column and the cursor and selection in the column that holds them (#1599).
- Gave each column its own line-number rail in column mode with word wrap, resolved mouse clicks per column, and added horizontal scrolling with Shift+wheel for column navigation (#1604, #1606, #1597).
- Fixed column mode never rendering in the real render and hit-test path, and a column-mode viewport performance regression (#1589, #1590).
- Opened a directory as a project root from the terminal Open dialog, where selecting a directory only descended into it (#1603).
- Discovered OS-installed Hunspell dictionaries, such as `hunspell-en-gb`, for zero-config spell check when `dictionary_paths` is empty (#1600).
- Added an "add to dictionary" action for the word flagged at the cursor, so one false positive no longer needs hand-editing the Accepted Words setting (#1596).

### 2026-09-19

- Added caret glide motion and a selection grow and settle animation (#1580, #1581).

### 2026-09-18

- Added panel scale-in and scale-out motion, with an easing and spring library and general tween infrastructure behind it (#1578, #1575).
- Added column mode (e-reader layout): a global toggle with target width and gap settings, viewport math, cursor placement, navigation, column rendering and a sweep animation (#1572, #1573).

### 2026-09-17

- Added the tab strip: geometry, composition, painting and hit-testing (#1558).
- Fixed comment lens, diagnostic highlighting, spell check, LSP cache, scroll, accessibility, command-runner and TUI mouse bugs; scrolling had assumed every pane fits the same number of rows regardless of its font (#1555).
- Retired the title-bar window sitter decoration in favour of docking its typing-reactive behaviour onto the companion sprite panel (#1556).

### 2026-09-16

- Made line numbers configurable by side (left, right or both) with margins and padding in cells, independent of interface density, and scaled prose font size (#1544).
- Made the interaction model cohesive: one context, one status line, settings grouped by task and the palette aware of context (#1546).
- Fixed the terminal read loop discarding end-of-input and expiry results while draining buffered bytes, which could drop characters typed in a burst on kitty (#1554).

### 2026-09-14

- Added Alt+Backspace as an alternate for delete word backward (#1320).
- Added a return-to-start-page command (#6).
- Made the paint worker thread a daemon (#1543).

### 2026-09-13

- Added fuzzy subsequence search and most-recently-used ranking to the command palette, opened it to recents, and gave it an explicit no-results state (#1492, #1493).
- Made semantic tokens the only syntax highlighting, on a new semantic tokens protocol layer (#1505, #1520).
- Added incremental LSP `didChange` sync, falling back to full text (#1494).
- Moved grapheme walking, word-wrap line breaking (UAX #14) and character width onto ICU4J (#1516, #1523, #1521).
- Added a TUI accessibility bridge (#1496).
- Collapsed the three command-palette density settings onto one Interface Density setting (#1487).
- Added numeric stepping and one-key reset to settings with no typing required, and normalised inconsistent value ranges and input paradigms across them (#1490, #1489).
- Painted the gutter and body divider as one continuous stroke (#1488).
- Made session-file and index writes crash-atomic (#1497).
- Fixed a selection-highlight redraw that silently dropped a run's font style (#1486).
- Added a win32-input-mode fallback for Ctrl+Backspace in Windows Terminal (#1504).
- Made `UndoState.boundedPush` O(1) amortised at steady state, and scoped terminal diff scans to the rows that changed (#1538, #1509).
- Preserved DOS attributes on Windows when saving (#1519).

### 2026-09-12

- Copied only POSIX permissions in the atomic file writer rather than the whole file's attributes (#1502).
- Reported DOCX and ODT decode failures through the return type instead of throwing (#1495).

### 2026-09-11

- Implemented free-form `COMPOUNDFLAG` compounding and `CHECKCOMPOUND*`, `CHECKCOMPOUNDPATTERN` and `ONLYINCOMPOUND` validation in the spell checker (#1437, #1438).
- Dropped legacy config-key compatibility (#1479).

### 2026-09-10

- Docked pinned panels in the workspace tree as the sole source of truth, and persisted the workspace tree and dock topology in UI presets (#1399, #1398).

### 2026-09-09

- Made pane splitting and closing reachable as user features, closing a pane undoable, and panel pin and unpin undoable (#1393, #1394, #1395, #1396).
- Added an explicit modal dialog layer outside the UI surfaces (#1397).

### 2026-09-07

- Fixed the editor pane zooming out and shedding ghost glyphs a little more on every Up or Down keystroke in the command runner, by blitting whole-surface layer buffers 1:1 instead of through the cell grid and device-scale transform (#1352).
- Fixed arrows and kitty-style Backspace splitting into Escape plus literal characters on non-tty input such as git bash on Windows without winpty, by timing the lone-ESC decision with a Cats Effect timer rather than JLine's timed read, and translated JLine's Windows `0xFFFF` Backspace quirk at the read boundary (#1352).
- Fixed the GUI hanging after the in-app Quit, which exited only through the window's close control, and stopped late resize and focus callbacks during shutdown being logged as crashes (#1352).

### 2026-09-06

- Added the panel framework: registration, a palette toggle, corner-stack layout, and command and keyboard resize (#1317).
- Added a mode and tab-list corner widget with a glyph indicator and tab switcher (#1312).
- Added a toggleable companion sprite pane in the GUI and TUI with visual flair levels (#1290).
- Parsed launch arguments with decline, and added `--help` and `--version` (#1318).
- Decoded SS3 application-cursor-key arrows and Home and End (#1322).
- Fixed the TUI open-file dialog hanging and being invisible when opened from the start page (#1321).
- Told the LSP server to stop when we stop waiting for it (#1315).

### 2026-09-05

- Added a Typewriter Scrolling setting (`display.typewriter_scrolling`, default off, plus a Text Display settings row and command-palette toggle): turning it on keeps the cursor's line at its centred row even while typing at the document's actual end, padding past it with blank rows, instead of falling back to the existing "show as much real content as fits" clamp. The clamp was silently overriding vertical centring during ordinary end-of-document typing -- the case #1204 was meant to cover -- and the mode had no config, command, or settings entry at all (#1293).
- Stopped a click on the floating cursor info bar from placing the caret in the hidden text behind it: the bar is derived per frame rather than stored, so the guard that keeps clicks out of a floating surface never saw it, and the bar sits exactly where the reader is working.
- Made Shift+Home and Shift+End select to the same place Home and End move to -- the cursor's own visual row under word wrap, rather than the whole logical line.
- Added Shift+PageUp and Shift+PageDown, which select a screenful of visual rows; the keys decoded with their modifier but had no binding at all, so they did nothing.
- Kept screen cells and buffer columns apart when grouping an animated run by colour, so a colour group after a wide glyph starts on the right cell and a surrogate pair is never split across two groups.
- Kept the terminal's cursor on the editing position in breathe mode, instead of parking it at the bottom of the screen on every frame that was not an idle cursor tick.

### 2026-09-04

- Property-tested the config format over generated settings, and fixed what it found: an explicit text-scale mode of "off" being overridden to "manual" by the multiplier, blur radius and background style never being written at all, and percentage settings coming back a floating-point hair away from what was saved.
- Applied broader config settings before the narrower ones that refine them, rather than in whatever order the key names happened to sort, which is what kept a motion preset from wiping the per-family settings saved alongside it.
- Moved the three config keys that were both a value and a path (`character.animation`, `ui.motion`, and each motion family's `animation`) onto leaves of their own, so none of them depends on being quoted to survive; the old spellings are still read.
- Measured the TUI's cell grid in display cells rather than characters, so the caret, word wrap, mouse hit-testing and cursor movement agree with the two cells a wide glyph is actually painted across.
- Widened Transport and Map Symbols and Symbols and Pictographs Extended-A to two cells, so a rocket or a plaster no longer takes one cell in the app's arithmetic and two on screen.
- Kept a terminal drag's selection when the mouse button is released, matching the Swing path, instead of collapsing it with a click synthesised from the release.
- Kept Home and End on the wrapped row the cursor is on, so End no longer leaves the caret at the far left of the row below and Home returns to the row's own start.
- Led the inert-in-TUI settings hints with their annotation, so it stays legible instead of being elided with the rest of a long hint.
- Moved PageUp and PageDown by a screenful of visual rows rather than logical lines, so a page through wrapped prose is a page of what is on screen, and left the viewport to follow the cursor at the effect boundary instead of the reducer scrolling on its own.
- Kept each render surface's previous-frame state against that surface rather than sharing one copy between all of them, and restored parallel test execution.
- Fixed a saved configuration with two or more cursor info bar segments writing a file that could not be read back, which silently reset every other setting to its default on the next launch.
- Refused to write a config file that cannot be parsed, so a formatting fault in one setting can no longer cost the user the rest of them.
- Persisted line numbers, gutter, word count, comment display mode, minimum pane width, command-runner key hints and the cursor-peek settings, none of which had a config key, and parsed `display.visual_line_navigation`, which had one but was never read.
- Kept an unreadable config file aside and said so on the start page, instead of a log line the TUI discards.
- Added a configurable mouse-wheel scroll distance (`input.wheel_scroll_lines`, default 3) and made the wheel scroll at all: both shells decoded wheel reports and dropped them.
- Repainted editor content on every damaged frame while the window sitter is active, so typed characters and their wrapped reflow appear immediately instead of trailing the cursor by the sitter's activity window.
- Anchored the vertical-navigation geometry window on the cursor's own visual row, so Up/Down and Home/End keep stepping by visual row deep inside a paragraph longer than a screenful instead of falling back to logical-line movement.
- Added a Visual Line Navigation toggle to the Text Display settings group, alongside the existing `display.visual_line_navigation` config key and command-palette toggle.
- Added a cell-level TUI behaviour suite driving real terminal sessions end to end (startup, editing, files, settings, unicode width, redraw, wrapped navigation, typing latency).
- Ran test suites serially, since the renderer's previous-frame state is keyed by pane id alone and concurrent painting suites overwrote each other's frame history.

### 2026-07-06

- Added a storage-location classifier that recognizes local paths, local `file:` URIs, and remote URI-backed document locations.
- Documented that remote storage is discoverable but not yet openable or saveable through the current local file IO.
- Blocked remote URI open/save-as workflow submissions before filesystem path parsing, keeping the modal open with a clear unsupported-storage status.
- Added parent breadcrumbs to nested settings search result rows in the command runner so preset settings are discoverable from search.
- Hardened Swing input shutdown observation tests so loaded Windows release jobs do not fail before idle streams can terminate.
- Changed the default window chrome to native OS chrome while keeping Serenity custom chrome available as an opt-in mode.
- Added release-cycle notes so the latest desktop release includes a human-readable changelog section alongside recent commits and downloadable assets.
- Kept stacked command-runner submenus contained inside the active editor content area in tiny viewports.
- Added a tested custom-chrome canvas fallback snapshot so editor layout receives post-title-bar viewport dimensions.
- Hid command-runner panel order controls unless multiple pinned panels share the same edge.
- Kept the selected theme-creator field inside the visible list window when editing near the end of the theme settings.
- Added an in-app UI outline thickness setting for panel and command-runner surface borders.
- Allowed pasted clipboard text to populate command-runner search and focused setting input rows.
- Made root settings search open matched submenus filtered to the matching nested setting row.
- Highlighted settings submenu breadcrumb ancestors and dimmed inactive command-runner panels while a child panel has focus.
- Preserved custom character animation duration and step settings across config reloads.
- Made quit-scope "Close anyway" discard the current dirty buffer before completing shutdown.
- Kept fast rendering active when text input arrives during a previous render phase shutdown, reducing cursor flicker.
- Restored maximized custom-chrome windows when title-bar dragging begins, allowing monitor-to-monitor drags to continue.
- Published visible cursor full frames atomically so cursorless base frames do not flash between overlay updates.
- Preserved rich-text inline formatting when editing inside formatted words.
- Matched custom-chrome window controls to platform placement and order on macOS versus Windows/Linux.
- Rendered Markdown preview images at device scale on Hi-DPI displays to avoid blurry split and inline-lens previews.
- Hid impossible panel action commands from settings when no panel is pinned on that edge.
- Clipped measured overlay text and carets to the framed content rectangle.
- Filled the full Markdown preview raster with the selected preview background for short documents.
- Split preset rename controls from preset actions in the command runner settings flow.
- Added a configurable command-runner reveal choreography setting backed by the semantic transition planner.
