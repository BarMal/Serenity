# Handover: TUI freezes when opening the "Convictions of a Mage" startup entry

Branch: `fix/open-recent-file-startup-page-freeze`

## TL;DR
Selecting the **recent‑file** entry on the splash screen (the one labelled with the
file path `…/Convictions of a Mage`) makes the TUI stop responding to all input.
Root cause: the `OpenRecentFile` path loads the file but **does not dismiss the
startup‑page surface**, and `Renderer` short‑circuits to drawing the startup page
whenever that surface exists — so the editor is never painted, keystrokes go to a
hidden buffer, and the app looks frozen. It is **not** a deadlock.

The fix + a regression test are already applied on this branch (see below).
**They have NOT been compiled/tested locally** (laptop shut down mid‑session).
First action for the async session: build + run the new test.

## Why every previous "fix" missed it
The user described it as *"restore a previous session (the Convictions of a Mage
entry)"*, so four prior agents all worked the **session‑restore** command path
(#1258/#1259/#1260 + an uncommitted `isTuiMode`/`keyboardFidelityTier` change).
But the entry the user actually selects is the **recent‑file** entry, whose command
is `FileIntent.OpenRecentFile`, a completely different code path. Restore‑session
works fine; `OpenRecentFile` is the broken one.

Also: existing integration tests (`SessionResumeIntegrationSpec`,
`StartupPageIntegrationSpec`) drive `StateManager.applyEvent` directly and never
render, so they can't catch a render‑masking bug. The reproduction below drives the
**real** `TuiRuntime` over a JLine terminal.

## Root cause (with citations)
1. Splash "recent file" action is wired to `OpenRecentFile`, and its **label is the
   file path** — which is why the user calls it the "Convictions of a Mage" entry.
   `[1] src/main/scala/com/serenity/app/AppStartup.scala:58-73`
2. Enter on a splash entry dispatches the action's command **inline** on the input
   fiber: `ComponentResult.ExecuteCommand` → `interpretCommand`.
   `[2] src/main/scala/com/serenity/state/manager/StateManagerEventPipeline.scala:393-398`
3. `OpenRecentFile` → `directLoadFileEffect(path)`. This path **does not clear
   `runtime.uiSurfaces`**, so the `StartPage` surface survives.
   `[3] src/main/scala/com/serenity/state/manager/StateManagerEffectHandlers.scala:453-457` (pre‑fix)
   `[4] src/main/scala/com/serenity/state/manager/StateManagerEffectHandlers.scala:2370-2414` (directLoadFileEffect)
4. Every sibling startup→editor transition **does** clear it:
   - native open‑file dialog: `uiSurfaces = List.empty` before `directLoadFileEffect`
     `[5] .../StateManagerEffectHandlers.scala:2440`
   - restore‑session: `restoreSessionIntoCurrentViewport` sets `uiSurfaces = List.empty`
     `[6] .../StateManagerWorkflowCapability.scala:914-925`
   - new / default buffer: `createStartupSession` / `createDefaultStartupBuffer` clear it too.
5. The renderer draws the **start page instead of the editor** whenever
   `state.startPageSurface` is defined (clears viewport, renders start page, returns).
   `[7] src/main/scala/com/serenity/ui/renderer/Renderer.scala:1092-1100`
   → editor content is never shown; because the (stale) start page doesn't change,
   the ANSI diff emits ~0 bytes per keystroke ⇒ "totally frozen".

### Why the thread dump looked healthy
The app is genuinely idle after the load: input events still reach the editor
(`hasBlockingModal` is false — the StartPage surface is `Floating`, not `Modal`,
`[8] .../AppStartup.scala:124`, `[9] .../state/models/AppState.scala:552-571`), the
buffer changes, but the render is masked. No fiber crashes (so the app doesn't quit
via `superviseLoop`→`forceQuit`), no loop terminates. A healthy‑idle TUI and this
wedged state are indistinguishable in a `jstack` dump — that's the trap.

## The fix (already applied on this branch)
`src/main/scala/com/serenity/state/manager/StateManagerEffectHandlers.scala`,
`OpenRecentFile` case — dismiss the startup surface before loading, mirroring the
native open‑file dialog path:

```scala
case FileIntent.OpenRecentFile(path) =>
  IO.blocking(java.nio.file.Files.isRegularFile(path) && java.nio.file.Files.isReadable(path)).flatMap {
    case true =>
      updateState(state => state.copy(runtime = state.runtime.copy(uiSurfaces = List.empty))) >>
        directLoadFileEffect(path)
    case false => logger.warn(s"[STARTUP] Recent file is unavailable: $path")
  }
```

Scoped deliberately to the `OpenRecentFile` case (not inside `directLoadFileEffect`)
because `directLoadFileEffect` is also called mid‑session by the file‑browser panel
(`PinnedPanelComponent`) and `FileEventReducer`, where clearing all surfaces would
wrongly close panels. `OpenRecentFile` is only ever dispatched from the splash, so
clearing `uiSurfaces` there is safe (verified: only `AppStartup` constructs it).

Alternative if you prefer defence‑in‑depth: instead remove only the StartPage
surface inside `directLoadFileEffect`'s committed state
(`uiSurfaces.filterNot(_ is StartPage)`), which also covers any future startup
caller. Either is correct; the applied one is the minimal, idiom‑matching change.

## Regression test (already applied)
`src/test/scala/com/serenity/StartupPageIntegrationSpec.scala` — new test
*"dismiss the startup page (not leave it masking a hidden editor) when a recent file
is opened"*. Seeds a recent file, relaunches, selects the `recent:` entry, and
asserts `afterOpen.startPageSurface shouldBe None` (the discriminating assertion —
fails on the old code, passes on the fix), plus that the file content loaded and
subsequent typing mutates the buffer.

## How to verify
1. Unit/integration (fast, definitive):
   ```
   sbt "testOnly com.serenity.StartupPageIntegrationSpec"
   ```
   The new test must pass; confirm it FAILS if you revert the fix (assertion
   `startPageSurface shouldBe None`).  **Do not add a timeout to sbt** (per CLAUDE.md).
2. Full end‑to‑end against the real terminal (optional, high confidence):
   `sbt clean assembly`, then reproduce with the PTY harness in the appendix.
   Pre‑fix: after selecting the recent entry, every keystroke yields 0 output bytes
   and no editor content appears. Post‑fix: the editor renders and input responds.

## Pre‑existing uncommitted work bundled on this branch
The working tree already had an unrelated (and insufficient for this bug) change:
`restoreSessionIntoCurrentViewport` now preserves `isTuiMode` +
`keyboardFidelityTier` across restore, plus a new `SessionResumeIntegrationSpec`
test. It's a reasonable improvement (runtime fields shouldn't be lost on restore) but
does NOT fix this freeze — `keyboardFidelityTier` in `AppState` only drives a UI
warning; the decoder's tier is fixed at startup negotiation. It's committed here as a
separate commit so it isn't lost; keep or drop independently.

## Appendix: live reproduction harness
The bug only reproduces through the real terminal runtime (kitty tier is not even
required — Legacy repro also masks the editor). Session root is
`System.getProperty("user.home")/.serenity`, so an isolated instance can run against
a copy via `-Duser.home`. Minimal synthetic repro (no user data needed): create a
recent file, launch `--tui`, pick the recent entry, then observe 0‑byte responses.

Driver used locally (kitty‑tier, poll‑based) is reproduced below; point `FAKE` at a
home dir containing `.serenity/{session-index.json,sessions/session.json,config.conf}`
with a `recentFiles` entry, or seed one by opening a file once.

```python
# spawn `java -Duser.home=$FAKE -jar Serenity.jar --tui --alpha` on a pty,
# auto-reply b"\x1b[?1u" to the b"\x1b[?u" kitty query, wait for "Welcome to Serenity",
# send N x b"\x1b[B" to reach the recent entry, b"\x1b[13u" for Enter,
# then measure bytes produced by subsequent keys. 0 bytes on every key == frozen.
```
Observed pre‑fix (recent entry, 3 downs): `editor content appeared: False`, and
`arrow-right/arrow-down/type-Z/arrow-up/type-Q` each produced **0 bytes**.
Restore‑session (2 downs) produced normal output and stayed responsive.
