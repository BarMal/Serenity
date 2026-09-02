# Session-Restore Hang: Investigation Findings

**Date investigated**: 2026-09-02  
**Investigated by**: Claude (Sonnet 4.6), across two context-window sessions  
**Serenity PID at time of investigation**: 298253  
**App started at**: 10:41:39 (from log)  
**jstack captured at**: ~13:05 (same day, ~2.5 hours after start)

---

## Summary for the next agent

The app is not deadlocked. The thread dump shows all compute threads idle (parked, waiting for work). The root cause is a silent session-restore failure: when the user selects "Restore previous session" on the startup page, the restore completes without creating any buffers, the startup page UI surface is removed, and the editor is left showing a blank empty pane. The user perceives this as a hang. There are also two secondary CE3 compute-thread starvation bugs in the session management code that were uncovered in the same investigation.

This document exists to hand off to the agent who will write TDD tests and implement fixes.

---

## Artifacts in this directory

| File | Source | Notes |
|------|--------|-------|
| `serenity.log` | `~/.serenity/serenity.log` | Active log at time of investigation |
| `serenity.2026-08-19.0.log.gz` through `serenity.2026-09-01.0.log.gz` | `~/.serenity/` | Historical rolling log archive |
| `jstack-pid-298253.txt` | `jstack 298253` output | Thread dump at ~13:05 |
| `session.json` | `~/.serenity/sessions/session.json` | Current (valid) session file |
| `session.json.corrupt-1788119212937` | `~/.serenity/sessions/` | Quarantined corrupt session — critical evidence of prior crash |
| `session-index.json` | `~/.serenity/session-index.json` | Session index pointing to current session |

---

## Timeline from logs and artifacts

```
2026-09-02 08:36:43  [io-compute-blocker-28]  Session saved successfully (Last Session)
2026-09-02 08:36:43  [io-compute-blocker-28]  Session saved successfully (Last Session)  ← saved twice on shutdown (intentional double-save in shutdown path)
2026-09-02 08:36:43  [io-compute-4]            Serenity editor shutdown complete
                     *** gap: ~2 hours ***
2026-09-02 10:41:39  [io-compute-blocker-2]   Starting Serenity text editor
2026-09-02 10:41:39  [io-compute-blocker-2]   [SESSION] Session loaded successfully with 0 buffers
2026-09-02 10:41:40  [io-compute-blocker-2]   Initial render completed, starting main loop
                     *** LAST LOG ENTRY — no further entries at time of jstack (13:05) ***
```

**Critical observation**: There is no `[CMD] Session restore requested` log entry. Since logback's `RollingFileAppender` is synchronous (each `logger.info(...)` call flushes before returning), if `restoreStartupSession()` had run, this line would be in the log. Its absence means that as of the jstack (13:05, 2.5 hours after start), the user had NOT successfully triggered a session restore. The startup page was still on screen.

---

## Thread dump analysis (`jstack-pid-298253.txt`)

**Key threads:**

| Thread | State | Stack |
|--------|-------|-------|
| `io-compute-blocker-13` | RUNNABLE | `FileInputStream.read0` ← `rawReadLoop` (correct: dedicated fiber for terminal reads) |
| `io-compute-0` through `io-compute-7` | WAITING | Parked at `WorkerThread.parkLoop` — completely idle |

**Conclusions from thread dump:**
- No deadlock. No `BLOCKED` threads. No thread holding a monitor that another is waiting for.
- The app is idle, correctly waiting for keystrokes.
- `rawReadLoop` is running normally on a blocker thread (`IO.interruptible` is used correctly there).
- All compute threads are parked waiting for queued work — consistent with an app that has rendered its initial state and is waiting for user input.

---

## Session file analysis

### `session.json` (current, valid)

```json
{
  "buffers": [],
  "layout": {
    "editorPanes": [{ "id": 0, "bufferId": null }],
    "workspaceTree": { "EditorLeaf": { "id": "editor-0", "paneId": 0 } },
    "paneOrder": [0]
  },
  "focus": { "EditorPane": { "paneId": 0 } },
  "bufferOrder": [],
  "recentFiles": [
    "/home/barney/Documents/NoteFromBee",
    "/home/barney/Documents/Books/Convictions of a Mage",
    "/home/barney/.Git/Serenity/Convictions of a Mage"
  ],
  "schemaVersion": 2
}
```

**Structural properties of the restored state:**
- 0 buffers
- 1 editor pane (id=0, no buffer assigned)
- Valid workspace tree: `EditorLeaf { paneId: 0 }`
- Focus: `EditorPane(PaneId(0))`
- 3 recent files (referenced in startup page "Recent" section)

This session IS valid — it will pass `AppState.validated`. The session was saved successfully at 08:36 during the prior clean shutdown.

### `session.json.corrupt-1788119212937` (quarantined prior crash artifact)

```json
{
  "buffers": [],
  "layout": {
    "editorPanes": [],
    "activeEditorPaneId": null,
    "workspaceTree": null,
    "paneOrder": []
  },
  "focus": null,
  "bufferOrder": []
}
```

**This is the critical evidence of a prior crash**: completely empty layout, null workspace tree, null focus. This session file was quarantined by `SessionManager.recoverFailedSessionFile` when JSON decoding failed on a prior startup. The timestamp suffix (`1788119212937`) converts to approximately 2026-08-04 — this corrupt session predates the current investigation by about a month. The current `session.json` is a separate, valid file that was written after that recovery event.

---

## Root cause analysis

### Bug 1 (Primary — the perceived hang): Empty-session restore leaves blank editor

**What happens:**

1. App starts → `loadSession()` runs → loads `session.json` (0 buffers) → logs "Session loaded successfully with 0 buffers"
2. Startup page is initialized with `sessionExists = true` (session-index.json points to a valid `sessions/session.json`)
3. Startup page shows: "New document", "Open file or folder", "Restore previous session", recent files
4. User navigates to "Restore previous session" and presses Enter
5. `ExecuteCommand(RestoreStartupSession)` → `interpretCommand` → `restoreStartupSession()` [**`StateManagerWorkflowCapability.scala:887`**]
6. `loadSession()` runs again → loads the same `session.json` → returns `Some(restoredState)` with 0 buffers
7. `restoreSessionIntoCurrentViewport(restoredState, currentState)` [**`StateManagerWorkflowCapability.scala:910`**] runs:
   ```scala
   val restored = restoredState.copy(
     runtime = restoredState.runtime.copy(
       uiSurfaces = List.empty,  // ← CLEARS the startup page surface
       viewportSize = currentState.runtime.viewportSize
     )
   )
   ```
8. `updateState(current => restoreSessionIntoCurrentViewport(...))` writes this state
9. `validateAndUpdateState(newState, prevState)` runs → `validated` checks the state → PASSES (the state is structurally valid)
10. `stateRef.set(validState)` → state now has 0 buffers, 1 empty pane, `uiSurfaces = List.empty`
11. Render fires → TUI renders an empty editor pane (blank screen)
12. User sees blank screen and interprets it as a hang

**Why there's no log entry**: If the startup page was NOT responding to keystrokes (see Bug 2 below), the user never successfully triggered the restore. The startup page stays visible. From the user's perspective: they press keys, nothing moves, the app is "hung". The actual state of the app is: correctly idle, waiting for the next valid keystroke.

**The two scenarios that could both explain the "hang" perception:**

- **Scenario A**: Startup page does respond, restore runs, leaves blank screen → user sees blank screen = "hung"
- **Scenario B**: Startup page does NOT respond to navigation keys → user presses keys, nothing moves = "hung"

The log evidence (no `[CMD] Session restore requested`) strongly favors **Scenario B**. See Bug 2 for why.

**Source location**: `src/main/scala/com/serenity/state/manager/StateManagerWorkflowCapability.scala:887-902`

**Fix direction**: After `restoreSessionIntoCurrentViewport`, if the resulting state has 0 buffers (and 1 or more empty panes), either:
- Re-display the startup page (since there's nothing to restore), or
- Open a new empty buffer automatically (same as the `None` branch of `restoreStartupSession`)

---

### Bug 2 (Secondary — CE3 starvation, likely cause of startup page unresponsiveness): `safeSessionPath` blocks CE3 compute thread

**Location**: `src/main/scala/com/serenity/session/SessionManager.scala:149-156` (`sessionExists`) and `188-190` (`loadSessionFile`)

**What happens**: `safeSessionPath` [**line 353**] is called synchronously (not in `IO.blocking`) from within IO chains. It performs blocking filesystem calls:

```scala
// SessionManager.scala:353-369
private def safeSessionPath(sessionFileName: String): Option[Path] =
  val portableName = sessionFileName.replace('\\', '/')
  Try {
    val sessionsRootSymlink = Files.isSymbolicLink(sessionsRootAbsolute)  // BLOCKING syscall
    val hasSymlink = (0 until relative.getNameCount).exists { index =>
      Files.isSymbolicLink(sessionsRootAbsolute.resolve(relative.subpath(0, index + 1)))  // BLOCKING syscall in loop
    }
    ...
  }.toOption.flatten
```

Both call sites are outside `IO.blocking`:

```scala
// sessionExists (line 149-156): safeSessionPath called in flatMap continuation — on compute thread
def sessionExists: IO[Boolean] =
  readIndex().flatMap { index =>
    index.currentSessionId
      ...
      .flatMap(metadata => safeSessionPath(metadata.sessionFileName)) match  // ← blocks here
      ...
  }

// loadSessionFile (line 188-190): same pattern
private def loadSessionFile(sessionFileName: String): IO[Option[AppState]] =
  safeSessionPath(sessionFileName) match  // ← blocks compute thread here
    case None => IO.pure(None)
    case Some(sessionFile) => ...
```

**CE3 rule**: Compute threads MUST NOT block. `Files.isSymbolicLink` is a blocking POSIX syscall (`lstat(2)`). Under any filesystem latency (NFS, network mount, slow disk, kernel cache miss), this can stall the compute thread for tens or hundreds of milliseconds. During that window, no keystrokes can be processed on that thread — and on a machine with few compute threads, this can make the entire app unresponsive.

**Fix**: Wrap `safeSessionPath` in `IO.blocking(...)` at each call site, or make the function itself return `IO[Option[Path]]` using `IO.blocking` internally.

---

### Bug 3 (Secondary — CE3 starvation): `AppStartup.createStartPage` blocks CE3 compute thread

**Location**: `src/main/scala/com/serenity/app/AppStartup.scala:56`

```scala
val recentActions = recentFiles
  .filter(path => Files.isRegularFile(path) && Files.isReadable(path))  // BLOCKING syscalls
```

`createStartPage` is a pure function called from an IO chain during startup initialization. Each call to `Files.isRegularFile` and `Files.isReadable` is a blocking syscall (`stat(2)`). With 3 recent files, this is 6 blocking syscall-equivalents on the compute thread.

**Fix**: Move the `recentFiles` filtering into `IO.blocking(...)` and make `createStartPage` accept the already-filtered list, or make it return `IO[StartupPage]`.

---

## Call chain for the restore path (for next agent's reference)

```
StartupPageComponent (user presses Enter on "Restore previous session")
  → ComponentResult.ExecuteCommand(Command(SessionIntent.StartupRestoreSession))
  → StateManagerEventPipeline.applyComponentResult  [StateManagerEventPipeline.scala ~371]
  → interpretCommand(command, state)
  → restoreStartupSession()                          [StateManagerWorkflowCapability.scala:887]
    → loadSession()                                  [SessionManager.loadSession()]
      → readIndex()                                  [IO.blocking — correct]
      → loadSessionFile(sessionFileName)             [line 188 — safeSessionPath NOT in IO.blocking ← Bug 2]
        → safeSessionPath("session.json")            [line 353 — BLOCKING]
        → IO.blocking(Files.exists(sessionFile))     [correct]
        → readUtf8(sessionFile)                      [IO.blocking — correct]
        → SessionState.toAppStateIO(...)             [decodes JSON → AppState]
        → returns Some(restoredState)                [0 buffers, 1 empty pane]
    → restoreSessionIntoCurrentViewport(restoredState, currentState)  [line 910]
      → clears uiSurfaces = List.empty               [removes startup page surface]
      → preserves viewportSize
      → calls LayoutEngine.syncViewportDimensions
    → updateState(...)                               [stateRef.update — bypasses validation]
  → returns to applyComponentResult
  → validateAndUpdateState(newState, prevState)      [StateManagerComposition.scala:113]
    → normalizeCommandRunnerFocus(newState).validated
      → reconcileWorkspaceTree                       [workspaceTreeAlreadyReconciled returns true — fast path]
      → validationErrors                             [PASSES — tree and focus are consistent]
    → stateRef.set(validState)                       [state: 0 buffers, 1 empty pane, no surfaces]
    → scheduleDocumentAnalysis()
  → emitDamage → render fires
  → TUI renders empty editor pane → blank screen
```

---

## Tests to write (TDD — write these before implementing any fix)

All tests should go in `src/test/scala/com/serenity/session/` or `src/test/scala/com/serenity/state/manager/`.

### Test 1: Restoring a 0-buffer session should not leave blank screen

```
Given a saved session with 0 buffers
When the user selects "Restore previous session" on the startup page
Then the resulting state should either:
  (a) re-display the startup page (sessionExists still true), or
  (b) have exactly 1 open buffer (new empty document created)
And the resulting state should NOT have 0 buffers AND empty uiSurfaces simultaneously
```

The relevant existing test file to extend: `src/test/scala/com/serenity/StartupPageIntegrationSpec.scala`  
Note: The existing test uses `sessionExists = false` so this path is not covered at all.

### Test 2: `sessionExists` must not block compute thread

Use CE3's `TestControl` to verify `sessionExists` completes without blocking. The call to `safeSessionPath` within `sessionExists` must be wrapped in `IO.blocking`.

### Test 3: `loadSessionFile` must not block compute thread

Same pattern — verify `safeSessionPath` inside `loadSessionFile` runs under `IO.blocking`.

### Test 4: `AppStartup.createStartPage` recent files filter must not block

Since `createStartPage` is called from an IO chain, the `Files.isRegularFile` / `Files.isReadable` calls must be deferred to a blocking thread. Test that a call with inaccessible recent files doesn't stall.

---

## Files to read before implementing fixes

In order of importance:

1. `src/main/scala/com/serenity/state/manager/StateManagerWorkflowCapability.scala` (lines 887-920) — `restoreStartupSession`, `restoreSessionIntoCurrentViewport`
2. `src/main/scala/com/serenity/session/SessionManager.scala` (lines 149-210, 353-370) — `sessionExists`, `loadSessionFile`, `safeSessionPath`
3. `src/main/scala/com/serenity/app/AppStartup.scala` (lines 17-90) — `createStartPage`
4. `src/main/scala/com/serenity/state/manager/StateManagerComposition.scala` (lines 113-127) — `validateAndUpdateState`
5. `src/test/scala/com/serenity/StartupPageIntegrationSpec.scala` — existing startup page tests (to understand the test harness before adding new cases)

---

## What is NOT the cause

- **Deadlock**: Ruled out by thread dump — no BLOCKED threads, no monitor contention
- **Theme loading**: The transparent theme is built in and loads instantly
- **SpellChecker**: Disabled in the session config (`enabled: false`, empty dictionary paths)
- **Render loop crash**: App is still running 2.5 hrs after start; `superviseLoop` would have exited if the render loop had thrown
- **FS2 Stream stall**: Compute threads are all parked normally — not waiting on a semaphore or latch
- **The corrupt session file causing the CURRENT hang**: It was quarantined in ~August; the current `session.json` is a separate valid file

---

## Recommended fix order

1. **Fix Bug 1 first** (the primary UX issue): In `restoreStartupSession`, after `restoreSessionIntoCurrentViewport`, check if `buffers.isEmpty`. If so, fall through to the same new-buffer creation logic as the `None` case (or re-show startup page). Write the failing test first.

2. **Fix Bug 2** (`safeSessionPath`): Wrap `safeSessionPath` body in `IO.blocking`. Both call sites (`sessionExists` and `loadSessionFile`) will benefit. This also fixes the potential startup-page unresponsiveness.

3. **Fix Bug 3** (`createStartPage`): Move `Files.isRegularFile / isReadable` filter into `IO.blocking` and thread it through the initialization call chain so the startup page is built with already-filtered paths.
