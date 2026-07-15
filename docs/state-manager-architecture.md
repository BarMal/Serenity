# State manager component boundaries

`StateManager` is a façade over independently owned components. Components must receive only the
state, services, and collaborator operations they use; they must not receive the public manager or
a catch-all runtime object.

The composition root owns concrete infrastructure construction. It adapts that infrastructure to
the following boundaries:

- editor state and pane operations;
- surface and viewport operations;
- file and session operations;
- workflow operations;
- reducer event processing; and
- effect-family handlers.

Event processing applies a reducer result's state before interpreting its effects. Document-analysis
replacement cancels the previous analysis fiber before starting a replacement. Failures in optional
analysis and persistence work are logged at their owning boundary and do not replace valid editor
state.
