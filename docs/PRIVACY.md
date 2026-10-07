# Privacy

Serenity has no telemetry, analytics, crash reporting, update check or account. It opens no network connections of its
own, and nothing you write leaves your machine unless you send it somewhere yourself.

## What Serenity stores on your machine

Everything lives in a `.serenity` folder inside your home directory (`user.home`): `~/.serenity` on Linux and macOS,
`%USERPROFILE%\.serenity` on Windows.

| What | Where |
| --- | --- |
| Settings | `.serenity/config.conf` |
| Session: open files, layout, cursor positions, and the text of unsaved buffers kept so a crash or quit loses nothing | `.serenity/session-index.json` and `.serenity/sessions/` |
| Application log | `.serenity/serenity.log` (older logs rotate into compressed `serenity.<date>.<n>.log.gz` files) |
| Language server error output | `.serenity/logs/lsp-<language>.log` |
| Single-instance lock and the local channel a second launch uses to hand files to the running window | `.serenity/instance.lock` and `.serenity/instance.sock` |

The licence and notices and this statement are written, read-only, to a `serenity-licences` folder inside the system
temporary directory when you open them from the command palette.

Delete the folder to remove all of it.

## Network

- The only socket Serenity opens is a Unix-domain socket in `.serenity`, used between two copies of Serenity on the
  same machine. It is a file, not a network port.
- The Markdown preview does not load remote or `data:` images; only image files under the previewed document's own
  folder are drawn.
- Opening a remote (for example `sftp://`) location is not supported; Serenity does not connect to it.

## What other programs can do

Serenity starts programs only when you ask it to, and those programs are not covered by this statement:

- Language servers you configure are ordinary child processes launched from the command you set. Serenity talks to them
  over their standard input and output. Whether a server uses the network is up to that server.
- Project tasks you define run the commands you wrote.
- In the terminal (TUI) mode the clipboard is reached through a helper program: `wl-copy`/`wl-paste`, `xclip` or
  `xsel`.
- Spelling uses the dictionary bundled with Serenity and any Hunspell dictionary folders you configure or the system
  provides; they are read from disk only.
