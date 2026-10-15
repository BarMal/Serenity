# Menu bar

Serenity opens with no menu bar, so the window is the text and the cursor. Every command a menu holds is in the
command palette (`Ctrl+P`), so nothing is lost by leaving it hidden. Set `ui.menu_bar` in `config.conf`, or pick Menu
Bar under Interface Layout in the palette's settings, to change that. It applies after a restart.

| Value     | What you get                                                                                         |
| --------- | ---------------------------------------------------------------------------------------------------- |
| `hidden`  | No menu bar. This is the default.                                                                    |
| `alt`     | No bar until you press and release Alt on its own. Alt with another key, such as `Alt+F`, is left to the editor's own bindings. |
| `visible` | The bar is always shown.                                                                             |

With `alt`, a tap of Alt shows the bar and selects its first menu; Left, Right, Up, Down and Enter work as in any menu.
The bar goes away again when a command from it runs, when you press Escape, when you click elsewhere or when focus
returns to the editor. Tapping Alt again hides it too.

The menu bar exists on Windows and Linux. macOS keeps the application menu the system gives every window and Serenity
adds no menu of its own there, whatever `ui.menu_bar` says.

An unrecognised value is reported as an invalid entry when the config loads and the default (`hidden`) is used.
