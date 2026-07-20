# #735 packaged-artifact evidence

## Artifact

- Packaged-code head: `6c3594c1df45adda30f56dcaee442b9565a1fd23`
- Packaged app: Linux `jpackage --type app-image`, version `1.0.783`
- Assembly JAR SHA-256: `ee978527468101728101921764a4a3d494bee16d003147c46b0649188890be06`
- App-image tar SHA-256: `12f7de134167673fd35921c4cf08edf31ca5d983bb3c13d2dafe70376a0bcf96`
- Platform/tester: Linux container, Codex; virtual X11 display used only to observe the packaged desktop app.

## Isolated-home proof record

The packaged executable was launched under Linux/Xvfb and driven through the command runner. The retained Discard frames show a
dirty `Discard Proof` draft after its material preview and the subsequent `Preset draft discarded. Workspace restored.` result.
They replace the generic startup/runner images as transaction evidence.

| Evidence | SHA-256 |
| --- | --- |
| `preview-discard.png` | `fcca680f0108d0ecbffeaab78ae062b8118a89b4d92b37f0b568e938c932ad72` |
| `discard-restored.png` | `83b58cacc23bc0350a627a217461aa06e3f2d91bb531e566a8125913cbb852df` |

The retained Save/restart frames use a fresh isolated home and the custom preset `Save Restart Proof`. The initial
workspace used the default `Frosted` material. `preview-save.png` shows its draft previewed with `Clear`, and
`preset-saved.png` shows the `Preset saved. Configure Save Restart Proof.` confirmation. The app was then closed
normally and the same packaged executable was restarted against the same isolated home. `restart-startup.png` shows
that the previous session is available, and `applied-after-restart.png` shows the saved preset reopened after Apply
with `Material Preset` still set to `Clear`.

| Evidence | SHA-256 |
| --- | --- |
| `preview-save.png` | `51be930f742e1439dc744377d0d17004eb9496522d7be82dad86ac14c8a78daa` |
| `preset-saved.png` | `cba9431452dfbe3b9c6ecd6c3bfeea446b192ecd61e43af40557ee1b0f2dba9e` |
| `restart-startup.png` | `06893cdd8c178c40be7b3af5b9d8759a15a8299b319851cd93f06f00a65c3b38` |
| `applied-after-restart.png` | `7fcb1ef98ffc764e249440cc131db416bddf4c548df3e4a9d5083278e6af117c` |

## PR #790 follow-up record

- Packaged-code head: `eb9af9da99b3b390099c2597e1969871f3a8b90a`
- Packaged app: Linux `jpackage --type app-image`, version `1.0.790`
- Assembly JAR SHA-256: `01da7e882a5c0a9833c9eb7a201d9221ecd5debc34393624f686919920c05f96`
- App-image tar SHA-256: `b10bee826eaaa1734013264d4021ac29159aabe6c2076c651a53c02b9259b9a9`
- Platform/tester: Linux container, Codex; isolated `HOME` and Xvfb display `:101`.

The packaged app created `Save Restart Proof`, previewed its `Crystal` material, and explicitly saved it. The app
then exited normally, restarted against the same isolated home, and exposed `Save Restart Proof` through the Apply
Preset control. The process remained live after the preview and the save, and the display server was kept alive for
the entire launch/restart sequence.

| Evidence | SHA-256 |
| --- | --- |
| `790-preview-save.png` | `49486b36c7d3a1847acf205a081760176632fc13620d5d5c11516a24327e5141` |
| `790-preset-saved.png` | `2b60a52dca4b5ebced7c70a005eedb2b3f140ec03825d60258a1e139224f221b` |
| `790-restart-startup.png` | `538b82b30bb160d3fc046dcf3d43af58377d99cb29ee6b6da507bca3438ae201` |
| `790-applied-after-restart.png` | `4cd69913fd53c6182645cfd72274e6bb1374e65be2b89012ce70470624ec4ddb` |

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

The deterministic frame assertions are behavioral proof for the two flows. The packaged Preview → Discard and
Preview → Save → restart → Apply frames above are visual proof of both transactions. No application exit was
observed: the packaged process remained live through each action and exited normally when the window was closed. The
earlier blank frames came from ending the shell that owned the background Xvfb server, which removed the display rather
than exposing an application defect.
