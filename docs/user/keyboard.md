# Keyboard

Serenity's main modifier is `Cmd` on macOS and `Ctrl` on Linux and Windows. The table below lists the default global
bindings for each. Run **Toggle Shortcuts Help** from the command palette to see the bindings in force now, including
any you have changed and the editor's own movement and selection keys.

A terminal cannot deliver `Cmd` as an ordinary keystroke, so in the terminal interface a macOS default is replaced by
its `Ctrl` form.

## Default bindings

This table is generated from the default keymap by `KeyboardReferenceTable`, and a test fails when it is out of date.
Run `sbt "Test/runMain com.serenity.docs.KeyboardReferenceTable"` to rewrite it after a default changes.

<!-- keyboard-table:start -->
| Action | Config key | Linux and Windows | macOS |
| --- | --- | --- | --- |
| Save | `save` | `Ctrl+S` | `Cmd+S` |
| Quit | `quit` | `Ctrl+Q`, `EOF` | `Cmd+Q`, `EOF` |
| Undo | `undo` | `Ctrl+Z` | `Cmd+Z` |
| Redo | `redo` | `Ctrl+Y`, `Ctrl+Shift+Z` | `Cmd+Shift+Z`, `Cmd+Y` |
| Copy | `copy` | `Ctrl+C` | `Cmd+C` |
| Paste | `paste` | `Ctrl+V` | `Cmd+V` |
| Cut | `cut` | `Ctrl+X` | `Cmd+X` |
| Select all | `select_all` | `Ctrl+A` | `Cmd+A` |
| Toggle syntax highlighting | `toggle_syntax_highlighting` | `Ctrl+Shift+H` | `Cmd+Shift+H` |
| Open file | `open_file` | `Ctrl+O` | `Cmd+O` |
| Command palette | `command_palette` | `Ctrl+P`, `Ctrl+Ctrl` | `Cmd+P`, `Cmd+Cmd` |
| Contextual toolbar | `contextual_toolbar` | `Ctrl+Shift+T` | `Cmd+Shift+T` |
| New tab | `new_tab` | `Ctrl+T` | `Cmd+T` |
| Close tab | `close_tab` | `Ctrl+W` | `Cmd+W` |
| Split pane horizontal | `split_pane_horizontal` | `Ctrl+Alt+V` | `Alt+Cmd+V` |
| Split pane vertical | `split_pane_vertical` | `Ctrl+Shift+D` | `Cmd+Shift+D` |
| Close pane | `close_pane` | `Ctrl+Shift+W` | `Cmd+Shift+W` |
| File search | `file_search` | `Ctrl+Shift+F` | `Cmd+Shift+F` |
| Go to file | `go_to_file` | `Ctrl+E` | `Cmd+E` |
| Next tab | `next_tab` | `Ctrl+Tab` | `Cmd+Tab` |
| Previous tab | `previous_tab` | `Ctrl+Shift+Tab`, `Ctrl+ReverseTab` | `Cmd+Shift+Tab`, `Cmd+ReverseTab` |
| Move tab left | `move_tab_left` | `Ctrl+Shift+PageUp` | `Cmd+Shift+PageUp` |
| Move tab right | `move_tab_right` | `Ctrl+Shift+PageDown` | `Cmd+Shift+PageDown` |
| Find | `find` | `Ctrl+F` | `Cmd+F` |
| Replace | `replace` | `Ctrl+H` | `Alt+Cmd+F` |
| Go to line | `go_to_line` | `Ctrl+G` | `Cmd+G` |
| Save as | `save_as` | `Ctrl+Shift+S` | `Cmd+Shift+S` |
| Toggle shortcuts help | `toggle_shortcuts_help` | `F1` | `F1` |
| Focus left | `focus_left` | `Alt+Left` | `Alt+Left` |
| Focus right | `focus_right` | `Alt+Right` | `Alt+Right` |
| Focus up | `focus_up` | `Alt+Up` | `Alt+Up` |
| Focus down | `focus_down` | `Alt+Down` | `Alt+Down` |
| Toggle chapter ghosts | `toggle_chapter_ghosts` | `Ctrl+Shift+G` | `Cmd+Shift+G` |
| Open chapter note | `open_chapter_note` | `Ctrl+Shift+N` | `Cmd+Shift+N` |
| Open keyword note | `open_keyword_note` | `Ctrl+Shift+K` | `Cmd+Shift+K` |
| Toggle notes pin | `toggle_notes_pin` | `Ctrl+Alt+P` | `Alt+Cmd+P` |
| Bold | `bold` | `Ctrl+B` | `Cmd+B` |
| Italic | `italic` | `Ctrl+I` | `Cmd+I` |
| Select-all-occurrences | `select-all-occurrences` | `Ctrl+Shift+L` | `Cmd+Shift+L` |
| Select-next-occurrence | `select-next-occurrence` | `Ctrl+D` | `Cmd+D` |
| Skip-occurrence | `skip-occurrence` | `Ctrl+Alt+K` | `Alt+Cmd+K` |
| Underline | `underline` | `Ctrl+U` | `Cmd+U` |
<!-- keyboard-table:end -->

`Ctrl+Ctrl` and `Cmd+Cmd` mean pressing the modifier twice. `EOF` is the end-of-input key a terminal sends. The rows
after Toggle notes pin (bold, italic, the occurrence commands and underline) are commands; their config keys are written
`hotkey.command.<name>`.

## Several cursors

Select Next Occurrence (`Ctrl+D`) selects the word at the caret, then adds the next match of the selected text as another
cursor each time it is pressed, wrapping round the document. A selection that began as a bare caret matches whole words
only (`cat` does not match inside `concatenate`); a selection you made yourself matches wherever the text occurs.
Matching is case-sensitive. Skip Occurrence (`Ctrl+Alt+K`) drops the newest match and selects the one after it, and
Select All Occurrences (`Ctrl+Shift+L`) puts a cursor on every match, up to 10,000. Typing, deleting and pasting then
reach every cursor. Editors that bind skipping to the chord `Ctrl+K Ctrl+D` have no counterpart here yet, because a
binding is a single key; the three commands are in the palette and can be given any other key.

`Ctrl+D` used to split the pane side by side; that is now `Ctrl+Alt+V`. Toggle Notes Pin moved from `Ctrl+Shift+L` to
`Ctrl+Alt+P` for the same reason. A key you set yourself in `config.conf` is never taken over by a default: if you bound `Ctrl+D` to something else, the occurrence command simply starts unbound.

## Changing a binding

In `~/.serenity/config.conf`, give the config key from the table a list of triggers:

```
hotkey.save = ["ctrl+shift+s"]
hotkey.command_palette = ["alt+k"]
hotkey.command.bold = ["ctrl+b"]
```

A trigger is modifiers and a key joined with `+`. The modifiers are `ctrl`, `alt`, `shift` and `meta` (`cmd` and
`command` mean the same as `meta`). Two actions cannot share a trigger. An empty list unbinds a key. You can also
record a key from the command palette's settings.

The file lists only the bindings you have changed. Anything it does not mention keeps the default of the Serenity you
are running, so a default added in a later release reaches you without editing the file. Resetting a binding in the
settings removes its line. A file from before this change is read once with the lists that match the current defaults
(or hold only some of their triggers) treated as defaults; a trigger you had deliberately removed from such a list
returns, and removing it again keeps.
