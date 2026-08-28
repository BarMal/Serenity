# TUI Mode

Generated 2026-08-28 for issue #1112, the convergence point of epic #1103 (headless TUI mode).

Serenity runs as either a Swing GUI application or a real terminal application, over the same `AppRuntime` core. `Main` builds one of two capability bundles and closes `AppRuntime.run` over it; neither branch constructs the other shell's window/terminal type.

## Launch mode selection

`LaunchOptions.parse` recognises three bare flags in addition to the existing `--open`/`--file`/positional-path/`--eco` handling:

- `--tui` -- force the terminal shell.
- `--gui` -- force the Swing shell, even when auto-detection would pick the terminal. `--gui` wins if both are passed.
- neither -- auto-detect via `LaunchOptions.resolveTuiMode`, a pure function of the environment:

  > Use the terminal when no display is reachable (`$DISPLAY` and `$WAYLAND_DISPLAY` both unset or blank) **and** stdout is a real terminal (`System.console() != null`). Otherwise use the Swing GUI.

  Requiring stdout to be a real terminal, not just "no display", avoids auto-selecting raw terminal mode against a non-interactive invocation (a script piping stdout, a CI job with neither a display nor a pty) that could never supply keystrokes back. This is the one part of the rule the issue called out as "to be settled in review" -- the precise tradeoff is documented here rather than only in a comment.

`LaunchOptions.detectTuiByDefault`/`isDisplayReachable` are exposed as pure functions and covered directly by `LaunchOptionsSpec`, with no real I/O in the tests.

## The two capability bundles

| Capability | GUI (`Main.runGui`) | TUI (`Main.runTui` / `TuiRuntime.run`) |
|---|---|---|
| `makeInputHandler` | `SwingInputHandler` over the AWT canvas | `TerminalInputHandler` over the JLine terminal |
| `checkResize` / `registerResizeCallback` | `SwingWindow` resize hooks | `TerminalShell`'s `SIGWINCH` handling |
| `renderFull` / `renderCursorOnly` | Swing-specific `Renderer` entry points over `Java2DRenderSurface` | Surface-generic `Renderer` entry points (#1104) over `TerminalRenderSurface` (#1106/#1107) |
| `fileDialog` | `SwingFileDialog` (native chooser) | `None` -- falls back to the in-app save-as/open form (#1110) |
| clipboard | `SystemClipboard.awt` | `ClipboardStrategy.select` (#1111): AWT reuse when a display is reachable, OSC 52 through the terminal writer otherwise, an external CLI tool (`wl-copy`/`xclip`/`xsel`) next, an in-process clipboard as the last resort |
| `awaitExternalQuit` | `swingWin.awaitClose` (window close) | `TerminalShell.awaitExternalQuit` (`SIGINT`, i.e. Ctrl+C at the raw terminal) |
| device-scale provider | `swingWin.detectedDeviceTextScale` | inert stub, `IO.pure(1.0)` -- no device pixel ratio in a cell grid |
| preferred-window-size provider | `swingWin.currentPreferredWindowSize` | inert stub, `IO.pure(None)` -- no window to resize |
| `onFontConfigChanged` | resyncs Swing metrics | inert stub -- typography is inert in cell space |

`TuiRuntime` (`com.serenity.ui.tui.TuiRuntime`) owns the entire TUI bundle and is the only thing `Main`'s TUI branch calls into; it holds no reference to `SwingWindow`, and the GUI branch holds no reference to `TerminalShell`, so the two shells are structurally exclusive (`TuiLaunchWiringSpec`).

## Cursor rendering

The hardware terminal cursor is hidden once, for the whole TUI session, by `TerminalShell`'s raw-mode setup (#1107). What the renderer calls "the cursor" is drawn as an ordinary cell glyph through the same `renderCursorOnly`/`renderWithCursorOverlay` entry points the GUI path uses, so blink/breathe timing is identical across both shells -- verified by exercising a real edit/save/quit session against a JLine `DumbTerminal` in `TuiRuntimeSpec` rather than re-implemented.

## Inert config surfaces in TUI mode

Post-processing effects and typography (font family/size, ligatures, the proportional prose path) have no visible effect on a fixed-cell terminal surface (epic #1103's accepted degradations). Rather than hiding these controls -- which would make it impossible to prepare a config file while running headless -- the command runner's settings surface annotates their hint text with "-- inert in TUI mode" when it is running in TUI mode:

- The **Post-processing** option (`settings-surface-appearance`).
- The **Typography** group and its three children, **Code Font**, **Prose Font**, **UI Font**.

The flag reaches the command runner as `AppState.Runtime.isTuiMode`, a never-persisted field set once at startup from the launch mode (`AppStartup.initializeState`/`startPageState`, threaded from `AppRuntime.run`'s new `isTuiMode` parameter). `AppEventReducer.toggleCommandRunner` and `StateManagerComposition.ensureCommandRunnerSurface` pass `state.runtime.isTuiMode` into `CommandRunner.activate`, which carries it into `settingsGroups` and so into `CommandRunnerSettingsGroups.build`. See `CommandRunnerActivationSpec` and `CommandRunnerSettingsGroupsSpec` for coverage.

## AWT stays non-headless

`Main` never sets `java.awt.headless=true`. AWT clipboard reuse (#1111, used when a display is reachable even in TUI mode) and the planned Markdown preview window (#1113) both depend on a live, window-less AWT toolkit. `Font`/`FontRenderContext` usage (font metrics, `CellMetrics.fromFont`) is headless-safe and unaffected either way; `TerminalRenderSurface.fontRenderContext` is always `None`, which is what drives #1105's cell-fallback rendering path so no AWT window/toolkit API is ever reached from the TUI render path.

## A real, TUI-specific bug this integration found and fixed

Wiring `TerminalShell.awaitExternalQuit` into `AppRuntime.run`'s `coordinateExternalQuit` exposed a genuine hang: `coordinateExternalQuit` races `awaitExternalQuit` against `stateManager.awaitQuit` and cancels whichever side loses. On the ordinary keyboard-driven quit path (Ctrl+Q or EOF), `awaitQuit` wins and `awaitExternalQuit` is the side cancelled -- but `TerminalShell`'s original `awaitExternalQuit` was built directly on `IO.async_` with no cancellation finalizer, and cancelling that fiber never actually completed, hanging the whole runtime forever. `TerminalShell` now completes a `Deferred[IO, Unit]` from the `SIGINT` handler instead (via a `Dispatcher` acquired alongside the shell); `Deferred`'s waiter is removed from its join list on cancellation and returns immediately, matching the pattern `AppRuntimeSpec`'s own passing tests already used for their `awaitExternalQuit` sources. This was caught by `TuiRuntimeSpec`'s end-to-end edit/save/quit test, not by `TerminalShellSpec` in isolation, which only ever completes the `SIGINT` side of that race.

## Known degradations (epic #1103, accepted 2026-08-24)

- Typography controls (font family/size, ligatures, the proportional prose path) are inert in cells -- annotated above, not hidden.
- The Swing accessibility bridge does not carry over; terminal screen-reader support is out of scope.
- On terminals without the kitty keyboard protocol, bare-modifier and Ctrl+Shift-style bindings degrade silently (legacy pty encoding destroys the information before it reaches the process) -- see #1109.
- Blur/glow/shadows/rounded corners/inline preview imagery are visual-only losses; the terminal theme supplies ambience instead.
- Character-reveal animation state (`bufferAnimations`) does not yet reach the surface-generic `renderWithCursorOverlay`/`render` entry points #1104 built -- only the surface-generic `renderCursorOnly` accepts it. TUI buffers therefore render without the typing/reveal animation applied to full-frame repaints; the Swing-specific overloads are unaffected. This is a gap in #1104's surface-generic API, not something #1112 changed or is positioned to fix without touching `Renderer`'s established signatures.

## Manual verification

`serenity --tui somefile.md` in a real terminal: opens the file, accepts typed input, saves on the configured Save binding, and restores the terminal (alternate screen exited, cursor shown) on quit -- exercised automatically in `TuiRuntimeSpec` against a JLine `DumbTerminal`; a real terminal should behave identically since the wiring under test is exactly what `Main.runTui` uses.
