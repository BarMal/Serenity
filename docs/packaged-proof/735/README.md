# #735 packaged-artifact evidence

## Artifact

- Reviewed PR head: `1c34949acc72e6ec896ce29c3e4723091c770c5e`
- Packaged app: Linux `jpackage --type app-image`, version `1.0.783`
- Assembly JAR SHA-256: `ee978527468101728101921764a4a3d494bee16d003147c46b0649188890be06`
- App-image tar SHA-256: `12f7de134167673fd35921c4cf08edf31ca5d983bb3c13d2dafe70376a0bcf96`
- Platform/tester: Linux container, Codex; virtual X11 display used only to observe the packaged desktop app.

## Isolated-home repro record

The packaged executable was launched under Linux/Xvfb and driven through the command runner. The retained Discard frames show a
dirty `Discard Proof` draft after its material preview and the subsequent `Preset draft discarded. Workspace restored.` result.
They replace the generic startup/runner images as transaction evidence.

| Evidence | SHA-256 |
| --- | --- |
| `preview-discard.png` | `fcca680f0108d0ecbffeaab78ae062b8118a89b4d92b37f0b568e938c932ad72` |
| `discard-restored.png` | `83b58cacc23bc0350a627a217461aa06e3f2d91bb531e566a8125913cbb852df` |

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

The deterministic frame assertions are behavioral proof for the two flows. The packaged Preview → Discard frames above
are visual proof of that transaction. The packaged Preview → Save → restart capture is not claimed by this record:
the package exits after the startup action in a fresh Java-isolated home, and also exits when `Ctrl+P` opens the runner
in an isolated home seeded with a copy of a valid session. Both runs used `-Duser.home` and Linux/Xvfb. The resulting
packaged startup/runner failure remains required acceptance work; it must be fixed before a truthful Save/restart
recording can be attached.
