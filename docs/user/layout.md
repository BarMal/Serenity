# Layout

Serenity opens as a blank page: no line numbers, no pane headers, no status bar, no tab strip and no scrollbar. That is
the `minimal` layout, the default. The `conventional` layout is the chrome earlier builds showed. Set `ui.layout` in
`config.conf`, pick Layout under Interface Layout in the palette's settings, or run Toggle Layout from the command
palette (`Ctrl+P`) to switch. The change applies at once, in the window and in the terminal, with no restart.

## What each layout starts from

| Setting             | `minimal` | `conventional` | Summon it in `minimal`                                        |
| ------------------- | --------- | -------------- | ------------------------------------------------------------- |
| `editor.line_numbers` | `false` | `true`         | Toggle Line Numbers                                           |
| `editor.pane_headers` | `false` | `true`         | Toggle Pane Headers                                           |
| `status.placement`  | `off`     | `pinned`       | Toggle Status Line                                            |
| `editor.tab_strip`  | `off`     | `auto`         | Tab List shows the open files; `editor.tab_strip = auto` keeps a strip |
| `editor.scrollbar`  | `false`   | `true`         | `editor.scrollbar = true`                                     |

`editor.tab_strip = auto` shows the strip once two or more files are open; `off` never does.

With no file to open, both layouts start on the start page. Return to Start Page is in the command palette in both.

The menu bar (`ui.menu_bar`) and the title bar (`window.chrome`) are not part of the layout; see menu-bar.md.

## A layout is a starting point

The layout only chooses what those five settings start as. Any of them you write in `config.conf` wins, whichever
layout is set and wherever in the file it sits:

```
ui.layout = minimal
editor.line_numbers = true
```

That is a minimal layout with line numbers. Switching layouts later moves the settings you never wrote and leaves the
ones you did, and a setting you change from the palette counts as yours too. Saving after a switch writes `ui.layout`
and nothing else it supplied, so the file does not fill with values you never chose.

An unrecognised `ui.layout` or `editor.tab_strip` is reported as an invalid entry when the config loads and the
default is used.

Existing configs need no migration: a file that does not set these keys now opens in the minimal layout. Add
`ui.layout = conventional` to keep the earlier chrome.

## Summoning hidden chrome

A hidden element does not have to be switched on to be seen. Hold the **summon chord**, `Alt+Shift` and the element's
letter, and it shows for as long as you hold it; let go and it goes again.

| Element       | Letter | Config key               |
| ------------- | ------ | ------------------------ |
| Line numbers  | `L`    | `ui.summon.line_numbers` |
| Status bar    | `S`    | `ui.summon.status_bar`   |
| Tab strip     | `T`    | `ui.summon.tab_strip`    |
| Scrollbar     | `B`    | `ui.summon.scrollbar`    |
| Pane headers  | `H`    | `ui.summon.pane_headers` |

- **Lock it.** Press the lock key, `Space` by default (`ui.summon.lock`), while the chord is held and the element stays
  shown after you let go. A locked element stays shown across a restart, with the rest of the session.
- **Dismiss it.** Press a locked element's chord again and it hides, and stays hidden for the rest of that press.
- **Several at once.** Hold two chords and both show; `Space` locks every element you are holding.
- **Typing is unaffected.** The lock key is taken only while a summon chord is held. Every other key does what it
  always does, and neither the letter nor the `Space` is typed.
- **The config is never written.** An element shows if the config shows it, or it is summoned, or it is locked, so
  summoning cannot turn off something the config shows, and a locked element is part of the session, not of
  `config.conf`. The layout and the renderer read the same answer, so a row or column is reserved exactly while its
  element is painted.
- **The tab strip** still needs two or more files open, as it does when configured on.
- **The status bar** is the pinned bar at the bottom, with the segments `status.segments` lists, unless the config
  already shows a floating one.

### In the terminal

A terminal reports a key press but never its release, so there the chord **toggles**: the first press shows the element
and locks it, the next press hides it. The lock key is not used. A terminal that cannot report Shift with a letter
sends a capital, which the chord reads as `Alt+Shift`.

### Changing the chord

All of it is set in `config.conf`, and in the palette's settings under Interface Layout, Summon Chrome:

```
ui.summon.modifiers = "alt+shift"
ui.summon.lock = space
ui.summon.line_numbers = l
ui.summon.status_bar = s
ui.summon.tab_strip = t
ui.summon.scrollbar = b
ui.summon.pane_headers = h
```

`ui.summon.modifiers` is any of `ctrl`, `alt`, `shift` and `meta` joined with `+` and quoted in the file, as a hotkey
is (not `shift` on its own, which types capitals). A letter is one letter or digit, or `off` to give that element no chord. The lock key is `space`, `enter`,
`tab`, `backspace` or one letter or digit.

A chord is dropped, and reported like any hotkey conflict, when it cannot work: a letter two elements share goes to the
first, a chord a hotkey in `config.conf` already holds goes to that hotkey, and a lock key that is also a summon letter
goes back to `space`. The settings list refuses such a change and says so. The defaults take no default hotkey on
Linux, Windows or macOS (`Alt+Shift+A` is toggle-block-comment, and `A` is not a summon letter).

### Held keys and auto-repeat

The Linux window systems (X11 and XWayland) send a held key's auto-repeat as a release followed at once by a press of
the same key. A release is therefore not taken as final until 12 ms have passed with no press of that key, so a held
chord does not flicker. Windows and macOS send repeated presses alone, which the chord ignores. Releasing `Alt` or
`Shift` ends the hold at once, and so does the window losing focus.

`Alt` on its own still shows the menu bar when `ui.menu_bar = alt`; a summon chord is not a tap of `Alt`.
