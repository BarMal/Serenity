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

## When your config file already sets them

Earlier builds wrote every setting into a new `config.conf`, so an older file usually sets all five, and a key you wrote
wins over the layout. Serenity asks before that gets in the way, and shows what would change for each key as
`current -> layout`:

- **On a switch.** Toggle Layout, the Layout row in settings and `ui.layout` changes made from the palette stop to ask
  when the file sets a key to something the new layout would change. Nothing is switched until you answer, and Cancel
  (or Escape) leaves the layout as it was. If no key differs, the layout switches as before.
- **Once, on the first launch after upgrading.** If the file sets keys to values other than the active layout's, the same
  question appears over the start page or the file you opened. It is asked once, whatever you answer, and Escape counts
  as an answer: keep your settings. Serenity remembers that in `~/.serenity/layout-override-reviewed`, not in your
  config.

The answers are:

- **Use the layout for all** removes those keys from `config.conf`, so each follows the layout from then on, including
  the next time you switch. Only those lines are removed; your comments, ordering and other settings are untouched, and
  a copy of the file is kept beside it as `config.conf.bak-<time>` first.
- **Keep my settings** switches the layout and leaves the file as it is. (On the first launch this is the dismiss
  button.)
- **Choose one by one** opens the preset review, one checkbox per key: checked keys go back to the layout, unchecked
  keys stay as you set them. Apply Selected Changes finishes the switch.

A key written somewhere a line edit cannot reach, such as inside an `editor { ... }` block, cannot be removed; it is set
to the layout's value in place, so it stays pinned but says what you chose.
