# Target state architecture: single-writer update loop with laned effects

Status: agreed direction (#1697). `docs/state-manager-architecture.md` describes the current system; this document
describes where it is going and the rules every migration slice follows.

## Why

Today every capability holds the raw `Ref[IO, AppState]` (32 files, ~113 direct writes). Event dispatch reads a
snapshot, performs I/O, then `set`s a state built from that snapshot, while background fibers write the same `Ref`
outside the dispatch lock. Updates are lost (#1675), unvalidated writes commit invalid states (#1183), and the #1623
file watcher can open a reload-conflict modal between a save's disk write and its revision commit — the probable cause
of the `TuiRuntimeSpec` Ctrl+Q wedge (#1564/#1666).

## The model

```
 input / LSP / watcher / timers / effect results
                 │  (Msg)
                 ▼
        ┌──────────────────┐   pure: (Model, Msg) => (Model, List[AppEffect])   (Transition)
        │  Inbox (Queue)   │──▶ single dispatcher fiber ──▶ validate ──▶ Ref.set(model)   ◀── renderer reads
        └──────────────────┘                     │
                 ▲                               ▼  effects, in emission order
                 │                        ┌──────────────┐
                 └──── result Msgs ◀──────│ EffectLanes  │  I/O runs here, never under the dispatcher
                                          └──────────────┘
```

1. **One writer.** Only the dispatcher fiber writes state. Everything else — input, LSP, watcher, focus callbacks,
   timers, completed effects — *offers a message* to the inbox. No lock is needed because there is one consumer.
2. **Pure update.** Handling a message is a pure `Transition` (`StateT[Writer[Chain[AppEffect]], _, _]`, already in
   `state/reducers/Transition.scala`). No `IO`, no `Ref`, no clock or randomness inside it — time and random seeds
   arrive in messages.
3. **Validate every message.** After each message the dispatcher runs `AppStateValidation`; an invalid result is
   rejected (the previous model is kept) and logged. There is no unvalidated commit path.
4. **Effects run outside, in lanes.** The dispatcher hands the message's effects, in order, to `EffectLanes`. I/O
   never runs on the dispatcher fiber, so animation and input keep flowing during a save.
5. **Results come back as messages, versioned.** Every result message carries the version it was computed from
   (content revision, request id, generation). The reducer applies it only if still current; otherwise it merges
   metadata or drops it. This is the single rule that replaces all ad-hoc "applyIfCurrent" checks.

### Model

`Model` is what the dispatcher owns and the renderer reads:

```scala
final case class Model(app: AppState, undo: UndoState, bufferAnimations: Map[BufferId, AnimationState])
```

The separate `undoRef` and `bufferAnimationsRef` fold into it so a message updates all three atomically. Render-side
caches (`mouseTargetCacheRef`, #1677) are *not* model: they belong to the renderer.

### Messages

`Msg` is the existing `Event` union plus a new union member for effect results:

```scala
type Event = EditorEvent | AppEvent | SystemEvent | SurfaceEvent | MouseInputEvent | EffectResult
```

`EffectResult` cases live next to the effect family that produces them (e.g. `FileSaved`, `FileLoadFailed`,
`SearchResultsReady`, `AnalysisCompleted`, `ExternalChangeDetected`). Ticks are messages too
(`AnimationTick(nanos)`), coalesced so at most one is queued.

### Effect lanes

```scala
enum LaneKey:
  case File(path: Path)              // canonical path: two buffers on one file share a lane
  case Buffer(id: BufferId)
  case Lsp(language: LanguageId)
  case Directory(path: Path)
  case Search, Analysis, Theme, Config, Presets, Keybindings, Session, Project, Dialog, Timer

enum LanePolicy:
  case Sequential    // FIFO per key (saves, config writes)
  case SwitchLatest  // cancel the running job for the key, start the new one (search, analysis, preview commit)
  case DropIfBusy    // ignore the job if one is running for the key (dialogs)

enum Lane:
  case Keyed(key: LaneKey, policy: LanePolicy)
  case Inline        // no I/O: interpreted synchronously by the dispatcher (e.g. undo boundary bookkeeping)
  case Exclusive     // drain Sequential lanes, cancel SwitchLatest lanes, then run alone (quit, session restore)

trait EffectLanes:
  def submit(lane: Lane, job: IO[Unit]): IO[Unit]   // never blocks the caller on the job itself
  def drain: IO[Unit]                               // waits for all Sequential work; used by Exclusive and tests
```

Every `AppEffect` maps to exactly one lane through an exhaustive `match` (`AppEffect.lane`), so adding an effect
without deciding its concurrency is a compile error. Jobs run under a `Supervisor`; a failed job is logged and
reported back as a `…Failed` result message, never swallowed.

### Ordering rules

* Effects of one message are submitted in emission order. Within a Sequential lane they run FIFO; across lanes they
  run in parallel. A dependency across lanes (save, then LSP `didSave`) is expressed as a follow-up effect emitted by
  the reducer on the result message, never as cross-lane ordering.
* **Input during a pending load**: opening a file creates and focuses a buffer in `Loading(path)` state immediately;
  edits to a `Loading` buffer are rejected with a status message until `FileLoaded` arrives.
* **Edits during a save**: `SaveBuffer` captures `(content, contentRevision)`; `FileSaved(bufferId, contentRevision,
  diskRevision)` records the disk revision and marks the buffer clean only if its content revision still matches.
  A closed buffer ignores the result (#1671).
* **External changes**: the watcher offers `ExternalChangeDetected(path, diskRevision)`; the reducer ignores it while
  a save to that path is in flight or when the revision equals one this process wrote, which removes the
  save/watcher race.
* **Quit** is `Exclusive`: pending saves and config/session writes finish; search/analysis are cancelled.

## Rules for migration slices

* TDD: the failing spec comes first. Pure reducers are tested as plain functions of `(Model, Msg)`; lanes with
  `TestControl`; end-to-end behaviour with the existing `*UiScenarioSpec` harness.
* A migrated code path may not take a `Ref[IO, AppState]`. When a capability class no longer needs it, remove it from
  its port.
* No behaviour change beyond the ordering rules above; anything else is called out in the PR description.
* `ArchitectureChecks` gains, at the end of the migration: no `Ref[IO, AppState]` outside the dispatcher, and no
  `IO` in `state/reducers`.
