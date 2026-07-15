# State manager component boundaries

`StateManager` is a façade over independently owned components. `StateManagerImpl` expands the
construction-time runtime once and passes each component only its declared capability port; no
component receives `StateManager`, `StateManagerRuntime`, a self-type, or protected runtime
forwarding.

The composition root owns concrete infrastructure construction and is the only place where ports
are wired together. The former behavior stack is now assigned as follows:

- `StateManagerEditorFacadeBehavior`: editor state and pane operations;
- `StateManagerSurfaceFacadeBehavior` and `StateManagerViewportBehavior`: visible surfaces,
  panels, mouse targets, and viewport state;
- `StateManagerFileFacadeBehavior`: file façade operations;
- `StateManagerWorkflowBehavior`: file/session/config workflow decisions;
- `StateManagerEventPipelineBehavior`: reducer event routing and document-analysis scheduling;
- `StateManagerEffectHandlers` plus `CommandEffectInterpreter`: effect-family dispatch and
  ordered I/O interpretation.

Allowed direction is façade → composition root → capability port → owning capability. Reducers
remain pure; the event pipeline applies their resulting transition before sending each effect to
the command-effect interpreter. A port may point only toward the owner of an operation, never back
through the façade or an inherited sibling implementation.

Event processing applies a reducer result's state before interpreting its effects. Document-analysis
replacement cancels the previous analysis fiber before starting a replacement. Failures in optional
analysis and persistence work are logged at their owning boundary and do not replace valid editor
state.
