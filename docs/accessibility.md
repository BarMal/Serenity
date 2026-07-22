# Accessibility checks

Serenity paints its editor and surfaces with Java2D. The Swing accessibility bridge publishes the same stable pane,
surface, and control identities used by layout and hit testing, including focused document/surface state, selected
startup and command-runner controls, control values, and status text.

## Manual verification

Run these checks on each supported desktop platform with the native Java accessibility bridge enabled and a real
screen reader:

1. Focus the editor, command runner, settings, startup page, find/replace workflow, panes, and docked panels.
   Confirm the focused name and role change once per transition.
2. Move startup and command-runner selections. Confirm the selected action is named without cursor-position chatter.
3. Change a validation or status message. Confirm it is announced once.
4. Verify every mouse action can also be completed with the documented keyboard path. Compact density remains
   keyboard-first; comfortable and spacious density use the contract's two-row minimum target expectation.

## Known limitations

Automated tests verify the semantic projection and Swing `AccessibleContext` publication. They do not establish
screen-reader conformance: platform Java bridges and reader verbosity differ, so the manual checks above remain
required before claiming assistive-technology support for a release.
