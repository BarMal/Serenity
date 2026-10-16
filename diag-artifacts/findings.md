# Hold-to-summon chord diagnostic (Alt+Shift+L/S) on NixOS + Hyprland + XWayland

Diagnostic run only — no fix attempted. Worktree: additive, from `origin/master` tag
`v1.0.0-rc.3` (62f024e7). Machine: NixOS, Hyprland, XWayland (AWT has no native Wayland
toolkit on this JVM, so the Swing GUI runs as an XWayland client — see
`[TOOLKIT] PlatformDefault (Wayland session, but Oracle Corporation has no WLToolkit)`
in the run log).

## 1. Compositor / keyboard config

- `hyprctl getoption input:kb_options` → `str: [[EMPTY]]`, `set: false`. No keyboard
  remapping (no `altwin:swap`, `caps:swapescape`, etc.) in play.
- `hyprctl binds`: every bound entry uses `modmask` 64 (SUPER) or 65 (SUPER+SHIFT) only.
  Hyprland's modmask bit layout is SHIFT=1, CTRL=4, ALT=8, SUPER=64. **No bind in the
  compositor config uses bit 8 (ALT) at all.** Hyprland is not intercepting
  Alt+Shift+<letter> at the compositor level.
- `setxkbmap -query` (run under XWayland): `rules: evdev`, `model: pc105`, `layout: us`.
  Plain US layout, no AltGr/compose remapping.
- `~/.serenity/config.conf` has no `ui.summon.*` or `ui.layout` override — default summon
  bindings are active (confirmed later: `configModifiers=Set(Alt, Shift)`, the built-in
  default).

Conclusion: nothing at the compositor/keyboard-layout level explains the bug.

## 2. Does Serenity receive the chord? (xdotool / wtype injection)

Built `sbt assembly` from the worktree and ran it as a fully isolated instance
(`-Duser.home=<scratch dir>`, so it never touches the real `~/.serenity`, never contends
for the real instance lock, and never touches real documents — see isolation note below).

- **X11-level (`xdotool keydown alt shift l` / wait / `keyup l shift alt`)**: screenshot
  taken mid-hold shows **no line-number gutter**, and critically **no literal `l`
  character typed either** (word/char count unchanged). `screenshots/04-*.png`.
- **Wayland-level (`wtype -M alt -M shift -P l` / wait / `-p l -m shift -m alt`)**: same
  result — no gutter, no typed character. `screenshots/05-*.png`.

At this point the two outcomes are indistinguishable: "event never arrives" and "event
arrives, chord consumes it, but nothing renders" both look like nothing happening.
Added temporary stderr logging to `SwingInputHandler.keyPressed` (worktree-only, reverted
before pushing this branch — see `summon-diag-keyevents.log` for the captured output) to
tell them apart.

## 3. What Java actually sees (the key finding)

With logging in place, holding Alt+Shift+L via `xdotool keydown`/`keyup` produces (full
output in `summon-diag-keyevents.log`):

```
[SUMMON-DIAG] keyCode=76 modifiersEx=576 isAltDown=true isShiftDown=true isControlDown=false isMetaDown=false isAltGraphDown=false rawModifiersOf=Set(Alt, Shift) configModifiers=Set(Alt, Shift)
[SUMMON-DIAG] consumed=true
```

repeated for every auto-repeat press while held. This is **exactly correct**:
`modifiersEx=576` is ALT_DOWN_MASK(512)+SHIFT_DOWN_MASK(64) with no stray bits, no
`AltGraph`, no `Control`, no `Meta`. `SummonKeys.modifiersOf(e)` computes
`Set(Alt, Shift)`, which equals `inputRouter.summonConfig.modifiers` exactly.
`SummonChordTracker.pressed` returns `consumed=true` on the very first press of `L`
(with nothing else currently held), which per the tracker's own logic
(`SummonChordTracker.scala:62-65`) is only possible when `config.elementForLetter('l')`
resolved to `Some(ChromeElement.LineNumbers)` — i.e. the chord **is** recognized and a
`Signal.Held(LineNumbers)` **is** emitted.

So: **Hyprland/XWayland delivers the modifier state to the JVM with complete accuracy.**
This rules out the original hypothesis (an XWayland/Hyprland modifier-forwarding bug).
The input-handling layer (`SwingInputHandler`, `SummonChordTracker`, `SummonKeys`) is
working correctly end to end, on both synthetic-X11 and synthetic-Wayland injection.

## 4. Where it actually breaks: nothing paints

Despite `consumed=true` and the correct element being signalled, **no visible change ever
occurs** — tested three ways:

- Empty "Welcome to Serenity!" placeholder buffer (`screenshots/04`, `05`): no gutter.
- Real typed content (3 lines, so the normal editor paint path is active, not the
  empty-buffer placeholder): still no gutter, with the instrumented build confirming
  `consumed=true` on every repeat during the exact same hold (`screenshots/07`).

Traced the signal path in the source (read-only, no changes made beyond the reverted
diagnostic log lines):

- `SwingInputHandler` → `enqueueRaw(ChromeSummonPressed(element))` → event stream →
  `StateManagerEventPipeline.dispatchGlobalAppEvent` → `AppEventReducer.reduce`:
  `case ChromeSummonPressed(element) => ReducerResult.noEffects(summoned(state, _.pressed(element)))`
  (`AppEventReducer.scala:115-116`).
- `summoned` updates `state.runtime.summons.held` correctly
  (`ChromeSummons.pressed`, `ChromeSummons.scala:14-15`).
- `AppState.chrome` is a `lazy val` = `EffectiveChrome.of(persisted.config, runtime.summons)`,
  and `EffectiveChrome.of` correctly ORs the config value with
  `summons.shows(ChromeElement.LineNumbers)` (`ChromeSummons.scala:47`).
- `RendererChromePlan` does read `state.chrome.lineNumbers` to decide whether to paint the
  gutter (`RendererChromePlan.scala:285`).

So the wiring from keystroke to `AppState` looks correct on inspection. The one thing that
stands out: `AppEventReducer`'s `ChromeSummonPressed` case returns `ReducerResult.noEffects`
— i.e. the state changes, but no `AppEffect` (repaint/damage trigger) is produced.
**Leading hypothesis for a follow-up fix investigation**: the renderer's repaint/damage
scheduling for this event is effect-gated rather than driven by a general "did AppState
change" check, so a `noEffects`-only state mutation like this one never gets painted until
some unrelated event forces a repaint. This is a hypothesis from reading the code, not
confirmed by further instrumentation — the diagnostic brief was to determine whether
Serenity receives the chord (it does), not to fix the render path.

## 5. Isolation / safety notes

- Ran the GUI with `-Duser.home=<scratch>/diag-home` so `ConfigManager.defaultConfigPath`
  and `SessionManager.defaultSessionRoot()` (both `Paths.get(userHome, ".serenity")`)
  pointed at a scratch directory, never the real `~/.serenity`.
- `~/.serenity/config.conf` was backed up and md5-verified unchanged before and after the
  whole session (`ff047b31b43560df136682b9ab1e27cf`, both before and after).
- Mid-run, the diagnostic window briefly appeared to show the operator's own in-progress
  document ("Convictions of a Mage.rtf") — this turned out to be a second, independent
  live Serenity session the operator already had open, not a leak from the isolated test
  instance (confirmed: no session files were ever written under the scratch home, and the
  real file's mtime/md5 were unchanged throughout). The test instance was killed before
  any keystroke was sent to it, out of caution, and relaunched cleanly.
- The temporary `SwingInputHandler` stderr logging used for step 3 was reverted
  (`git checkout --`) before this branch was built; `git status`/`git diff` on the
  worktree are clean. It is not part of this push.
- No code changes are included in this branch. No PR opened.
