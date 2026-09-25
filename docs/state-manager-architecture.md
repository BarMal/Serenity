# State manager component boundaries

`StateManager` is a façade over independently owned components. `StateManagerImpl` expands the
construction-time runtime once and passes each component only its declared capability port; no
component receives `StateManager`, `StateManagerRuntime`, a self-type, or protected runtime
forwarding.

The composition root owns concrete infrastructure construction and is the only place where ports
are wired together. The former behavior stack is now assigned as follows:

- `StateManagerEditorCapability`: editor state and pane operations;
- `StateManagerSurfaceCapability` and `StateManagerViewportCapability`: visible surfaces,
  panels, mouse targets, and viewport state;
- `StateManagerFileCapability`: file façade operations;
- `StateManagerWorkflowCapability`: file/session/config workflow decisions;
- `StateManagerEventPipeline`: reducer event routing and document-analysis scheduling;
- `StateManagerEffectHandlers` plus `CommandEffectInterpreter`: effect-family dispatch and
  ordered I/O interpretation.

Allowed direction is façade → composition root → capability port → owning capability. Reducers
remain pure; the event pipeline applies their resulting transition before sending each effect to
the command-effect interpreter. A port may point only toward the owner of an operation, never back
through the façade or an inherited sibling implementation. Owner-local operations are invoked
directly and are never re-exported through the port that constructs their owner.

Commands that translate back into editor events and surface animation hooks enqueue typed operations
through `StateManagerOperationBoundary`. The event pipeline drains that boundary after each
interpreted effect or command, preserving synchronous FIFO ordering without giving effect handlers a
callback to the pipeline. File persistence is separately owned by `StateManagerFilePersistence` and
is shared directly by effect interpretation and file workflows. The dependency direction is therefore
event pipeline → effect handlers → capability port → operation boundary, while the event pipeline
alone consumes the operation boundary; effect handlers and their surface/workflow capabilities never
depend on the event pipeline or effect handlers.

Event dispatch is serialized by a single inbox (`StateManagerDispatcher`, first slice of
`docs/state-architecture-target.md`): one `Queue` consumed by a single dispatcher fiber that runs requests one at a
time. The fiber is started by the first offer into an idle inbox and exits when the inbox drains, so it needs no
owning `Resource`.
`applyEvent` and `updateStateValidated` offer their work and wait for it to be applied. Background work (find search,
markdown-preview commit, document analysis) runs off the dispatcher and *posts* its state update instead of writing
the state itself. Config, keybinding and UI-preset persistence commits its state decision once, validated, then writes
on the Sequential `Config`/`Presets` lanes (keybindings live in the config file, so they share `Config`); a preset
result comes back as an `EffectResult` applied only while still current. The external-change check (#1623) reads the disk off the dispatcher and decides on it, dropping an
observation that a save or reload has since superseded. Events enqueued while a dispatch interprets its effects are
replayed on the dispatcher by `drainPendingOperations`; code already on the dispatcher never offers-and-waits, which
would deadlock. The render tick advances animations only when the dispatcher is idle (`runIfIdle`); otherwise it
skips the frame's advance and reports still-active so the next frame retries. File reads and writes
(`StateManagerFilePersistence`) run on a `LaneKey.File` Sequential lane per canonical path and come back as
`EffectResult.FileSaved`/`FileLoaded`/`FileReloaded`, merged into the state current when they land: a save marks the
buffer clean only if its content is still the content written, and ignores a buffer closed meanwhile (#1671). A plain
save and a file open never wait on the disk on the dispatcher; save-as, save-before-close, the reload prompt's choices
and `FileOpener.openFile` wait for their lane job because their next step depends on its outcome. The external-change
check ignores a path while a save to it is in flight. The file, close, replace and session workflows decide in pure
transitions (`CloseWorkflowTransitions`, `FileWorkflowTransitions`, `SessionWorkflowTransitions`,
`ReplaceWorkflowTransitions`) and commit each step once, validated; a replace commits its edit and undo entry in one
model write. The Open/Save-As dialog lists directories on a `Directory` switch-latest lane and checks an Open target
on a `Directory` sequential lane; named-session list/save/rename/load run on the `Session` Sequential lane. Their
results are `EffectResult`s dropped once the dialog or picker they were computed for has moved on. Saving from the
unsaved-changes prompt closes the buffer only once the save has landed; a failed or conflicting save abandons the
close, quit included (#1708). Quitting -- normal or forced, e.g. closing the window -- is one
step, `shutdownEffects`: a `Lane.Exclusive` barrier lets every Sequential lane (file saves, config, presets) finish,
bounded by a grace period, cancels search and analysis, then releases the lanes. A project task runs on the
switch-latest `Project` lane: the state records the running task and refuses a second, cancelling supersedes the lane
with an empty job, quitting cancels the task (destroying its process), and its output reaches the dispatcher at most
once per 100ms as `EffectResult.ProjectTaskOutput`/`ProjectTaskFinished`, applied only while that task is still the
running one. The command runner's double-tap timer is a switch-latest `Timer` job posting
`CommandRunnerBindingExpired`; a result's follow-up effects (`EffectResult.reduce`) are interpreted on the dispatcher
after its commit. LSP traffic stays on `LspEffectQueue`, which is the `Lsp` lane already: one FIFO drained by
`LspManager`'s single consumer, with results returning through `applyEvent`. The dispatch pipeline itself still
performs I/O. No capability holds the model `Ref`: `StateManagerOperationBoundary` builds the one `ModelCommit` over
it, capabilities read the state through an `IO[AppState]` and write it only through `ModelCommit`, and
`ArchitectureChecks` rejects a `Ref[IO, AppState]` or `Ref[IO, Model]` outside that layer. Every state write goes through `AppStateValidation`
(a surface change commits its undo boundary in the same model write) except the render tick's animation advance and
the test-only `StateUpdater.updateState` seam.

Event processing applies a reducer result's state before interpreting its effects. Document-analysis
replacement cancels the previous analysis fiber before starting a replacement. Failures in optional
analysis and persistence work are logged at their owning boundary and do not replace valid editor
state.
