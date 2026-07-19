# #735 packaged-artifact evidence

## Artifact

- Release commit: `3b98109b8d474c3da63321ffd617613d1565aeeb`
- Packaged app: Linux `jpackage --type app-image`, version `1.0.783`
- Assembly JAR SHA-256: `ee978527468101728101921764a4a3d494bee16d003147c46b0649188890be06`
- App-image tar SHA-256: `12f7de134167673fd35921c4cf08edf31ca5d983bb3c13d2dafe70376a0bcf96`
- Platform/tester: Linux container, Codex; virtual X11 display used only to observe the packaged desktop app.

## Isolated-home repro record

The packaged executable was launched with a new `HOME` directory and no existing Serenity session. [startup.png](startup.png)
shows the resulting fresh-session chooser; after starting a session and pressing `Ctrl+P`, [command-runner.png](command-runner.png)
shows the packaged command runner. The images are retained with this record and have SHA-256 values recorded below.

| Evidence | SHA-256 |
| --- | --- |
| `startup.png` | `691407df876c502e8cab798c56b44715f735c1c0622f796abe5295476bc6d0e5` |
| `command-runner.png` | `e2a543b5def78f7c8cd7c1693cde85149cc2a9fff69e1e9103633783adf7470a` |

## Transaction scenarios

The real state/session path is exercised by `UiPresetUiScenarioSpec` with an isolated session root:

1. Create `Restart Draft`, preview the `Solid` material, save the session, construct a fresh runtime, restore the
   session through `StartupRestoreSession`, reopen the runner, then Discard. The test verifies the draft remains
   unsaved and that Discard restores the original material baseline.
2. Create `Scenario`, preview `Subtle` motion, Save, construct a new runtime, Apply the saved preset, and verify the
   saved workspace is restored.

Result: `testOnly com.serenity.UiPresetUiScenarioSpec` passed all 5 tests on this commit. No diagnostic PNGs were
emitted or retained: every rendered frame had an empty layout-violation set. The expected persistence-failure recovery
case logs `FileAlreadyExistsException`; it is handled by the test and does not represent a scenario failure.

The deterministic frame assertions are behavioral proof for the two flows; the screenshots above are the separate
packaged-app visual proof required by the checklist.
