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
