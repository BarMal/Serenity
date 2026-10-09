# Serenity

A calm text editor for writers and for code, written in Scala 3. It opens plain text and Markdown, edits in a window or
in a terminal, and can export a manuscript as DOCX or EPUB.

## Install

Builds come from the [`desktop-latest` release](https://github.com/BarMal/Serenity/releases/tag/desktop-latest), which
is rebuilt from `master` and marked as a pre-release. Each build is one of:

| File | Contents |
| --- | --- |
| `Serenity-linux-x64.zip` | App image for Linux. Unzip it and run `Serenity/bin/Serenity`. |
| `Serenity-windows-x64.zip` | App image for Windows. Unzip it and run `Serenity\Serenity.exe`. |
| `Serenity-macos-arm64.zip` | `Serenity.app` for Apple silicon Macs. Unzip it and open it. |
| `Serenity.jar` | Runnable jar for any OS with Java 21 or later: `java -jar Serenity.jar`. |

The app images carry their own Java runtime.

### From source

Install JDK 21 and [sbt](https://www.scala-sbt.org/), then from a checkout:

```bash
sbt run                    # start the editor
sbt "run notes.md"         # open a file
sbt assembly               # build target/scala-*/Serenity.jar
```

[DEVELOPMENT.md](DEVELOPMENT.md) has the build and check commands.

## First steps

1. Start Serenity. The start page offers a new document, **Open file** (`2`) and **Open folder** (`3`) as separate actions
   (a folder opens in the Explorer), your recent files, and the **Writing**, **Code** and **Compact** workflows (press `W`, `C` or `M`). If a session was saved, `Tab` resumes it.
2. Start typing. Press the command palette key (`Cmd+P` on macOS, `Ctrl+P` on Linux and Windows) and type to find any
   command. Every command can be run from the palette.
3. Run **Toggle Shortcuts Help** from the palette to see the current bindings, or read the [keyboard reference](docs/user/keyboard.md).
4. Save with `Cmd+S` or `Ctrl+S`.

With no display, or with `--tui`, Serenity runs in the terminal instead. `--gui` forces the window.

## Where things live

Serenity keeps everything under `.serenity` in your home folder on every OS (`~/.serenity` on Linux and macOS,
`C:\Users\you\.serenity` on Windows).

| Path | What it is |
| --- | --- |
| `~/.serenity/config.conf` | Your settings. |
| `~/.serenity/session-index.json`, `~/.serenity/sessions/` | The saved session and your named sessions. |
| `~/.serenity/serenity.log` | The application log. It rolls at 5 MB and keeps 14 days, 50 MB at most. |
| `~/.serenity/logs/` | Language-server logs. |

Serenity's own code makes no network connections, and has no telemetry or update check. Your files, settings,
sessions and logs stay on your machine. Language servers are separate programs that Serenity starts when you work in a
language that has one; they behave as their own authors made them. The full statement is in [docs/PRIVACY.md](docs/PRIVACY.md).

## If something goes wrong

Start with `--safe-mode` (or `--safe`) to run on default settings with no saved session. `--reset-config` and
`--reset-session` move your settings or your session aside to a timestamped backup and start fresh. Serenity also
starts in safe mode by itself after two starts that did not finish. See [safe mode and resets](docs/user/safe-mode.md).

## User guide

- [Keyboard](docs/user/keyboard.md): every default binding on each OS, and how to change one.
- [Spell check](docs/user/spell-check.md): on by default in British English; other languages and dictionary folders.
- [Sessions](docs/user/sessions.md): saving, resuming and naming sessions.
- [Exporting a manuscript](docs/user/export.md): DOCX and EPUB.
- [Safe mode and resets](docs/user/safe-mode.md)
- [The terminal interface](docs/tui-mode.md)

## Licence

Serenity is free software, released under the [GNU General Public License, version 3 or (at your option) any later
version](LICENSE) (SPDX: `GPL-3.0-or-later`).

It bundles third-party libraries and fonts under their own licences, listed with their copyright notices and full texts
in [THIRD-PARTY-NOTICES.md](THIRD-PARTY-NOTICES.md). That file is generated from the build (`sbt generateThirdPartyNotices`)
and checked in CI. The same text ships inside the application: run **Show Licence and Notices** from the command
palette. The bundled Monaspace font is under the SIL Open Font License; its text is at
[src/main/resources/fonts/OFL.txt](src/main/resources/fonts/OFL.txt).
