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

Event processing applies a reducer result's state before interpreting its effects. Document-analysis
replacement cancels the previous analysis fiber before starting a replacement. Failures in optional
analysis and persistence work are logged at their owning boundary and do not replace valid editor
state.
