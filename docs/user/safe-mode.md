# Safe mode and resets

Use these when Serenity will not start, or starts into something you want to get away from.

## Safe mode

```
Serenity --safe-mode        # or --safe
```

Safe mode starts with default settings and with the saved session, language servers, spell check and project tasks
turned off. It reads nothing from `~/.serenity` and changes nothing there, so leaving it returns you to exactly what you
had. The window title says it is running in safe mode, and the start page explains it. The session it writes while
running goes to a temporary folder that is removed on exit.

Serenity starts in safe mode by itself after two starts in a row that did not reach their first window. It then offers a
normal restart. The count lives in `~/.serenity/startup-in-progress`.

With the app images, pass the flag to the launcher (`Serenity/bin/Serenity --safe-mode` on Linux, `Serenity.exe
--safe-mode` on Windows). From source: `sbt "run --safe-mode"`.

## Resets

Resets move files aside; they never delete.

| Flag | Effect |
| --- | --- |
| `--reset-config` | Moves `~/.serenity/config.conf` to a timestamped backup beside it, then starts on default settings. |
| `--reset-session` | Moves the saved session to a `session-<timestamp>` folder in `~/.serenity`, then starts without it. |

Both can be given together. To undo a reset, move the backed-up file or folder contents back by hand. Settings,
presets and themes are not part of the session and are left alone by `--reset-session`.

Inside the editor, **Reset Settings** in the command palette backs up `config.conf` and restores every setting to its
default.

## Other launch flags

`--tui` forces the terminal interface, `--gui` forces the window (and wins over `--tui`), `--eco` lowers the frame-rate
target, `--alpha` enables experimental features, and `--version` prints the build and exits. Give a path, or
`--open path`, to open a file on start.

Logs for a problem report are in `~/.serenity/serenity.log`.
