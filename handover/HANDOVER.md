# Serenity orchestration handover — 2026-10-16

Previous session: https://claude.ai/code/session_01FFaijrmcykoATN664JcAzY (orchestrator).
This branch (`ccr-157ed55f-u6khhf-handover`) is for handover only. **Do not merge it into master.**

Files:
- `handover/HANDOVER.md` — this document.
- `handover/AGENTS-log.md` — the full running log, newest entries at the bottom.
- `handover/sbt-slot.sh` — the sbt wrapper.

## 1. Role and standing rules

### Role
- You orchestrate BarMal/Serenity, a Scala 3, Cats Effect and sbt text editor with a Swing/Java2D GUI and a TUI.
- The owner (Barnaby, GitHub BarMal) said "You own everything now".
- Priorities: a stable, high-performance core, then low memory use, then an immersive, cursor-centric UX.
- Be succinct, and lay problems out logically.

### Lean mode (owner request, 6 Oct)
- At most 2 Sonnet implementation agents plus 1 Sonnet triage agent at a time. Never use Opus agents.
- Delegate log-reading and fixes to agents rather than doing them yourself.

### CLAUDE.md (in the repo) is binding
- Work in two phases: explore with citations, then propose and wait for sign-off.
- TDD: write the failing test first and run it red.
- Never skip, disable, weaken or exclude a test.
- No TODOs or stubs.
- Never put a timeout on an sbt command.
- Never delete files or make destructive changes without explicit instruction.
- Be honest. Prefix speculation with "Caution, here be imagine dragons:".

### Lesson learned
- **Never ship visible UI without the owner's sign-off.** The menu bar in #2100–#2122 went in unapproved.
- Present a proposal as a numbered list and wait for approval.

### Commit trailers
```
Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: <your session URL>
```
- End PR bodies with "🤖 Generated with [Claude Code](https://claude.com/claude-code)" plus the session link.
- Put the Claude Code footer on any GitHub comment.

### Branches
- Agents push to `ccr-157ed55f-u6khhf-<topic>` branches.
- Each agent works in a manual worktree, `git worktree add .claude/worktrees/<topic> -b <branch> origin/master`, because the Agent tool's worktree isolation fails here.

### sbt
- Run every sbt call through `sbt-slot.sh`, which allows 3 slots machine-wide at 3 GB heap each.
- Copy it to your scratchpad. It expects sbt at `$HOME/tools/sbt/bin`.
- A full `test` needs `SBT_OPTS="-Xmx6g -Xss4m"`, otherwise it hits OOM.
- CI lint is `scalafmtAll "Compile / scalafix" "Test / scalafix"` followed by `git diff --exit-code`. Local `scalafmtCheckAll` alone misses scalafix import-order failures.
- Also run `architectureCheck`. It enforces three rules:
  - no mutable state in top-level objects;
  - methods of 80 lines or fewer;
  - files of 600 lines or fewer.

### CI behaviour
- **Auto-merge:** auto-squash-merge happens on a successful CI `workflow_run` for PRs that are not drafts. You only need to open a ready PR.
- **Undrafting doesn't rerun CI:** marking a PR ready does not trigger CI. Pushing does, and so does `update_pull_request_branch` when the branch is behind.
- **Perf gate:** it fails at 2x p50 plus a floor, or at more than 1.25x retained bytes and more than 1 MiB. Allocation is report-only.
- **Network failures:** Maven Central sometimes resets connections during `makeBom`. That is not a code failure. Re-run once.
- **Tags:** the session cannot push tags (403). The owner pushes release tags.

## 2. State at handover

- **`master` is at `62f024e7b`.** It contains #2277, the last merge.
- **No open PRs and no running agents.**

### Merged 14–16 Oct
- #2262 scrollbar: GUI overlay and TUI column, behind `editor.scrollbar`.
- #2265 white command-runner fix.
- #2267 `performance.memory.compact`: about 40% less resident memory (769 MB → 446 MB idle on the laptop).
- #2268 GTK file-chooser fallback (the Ctrl+O SIGABRT).
- #2269 menu bar hidden by default (`ui.menu_bar`).
- #2270 LSP flaky-test fix.
- #2271 `ui.layout = minimal | conventional`, default minimal. The start page is kept in both.
- #2272 hold-to-summon chrome:
  - Hold `Alt+Shift` plus `L`, `S`, `T`, `B` or `H` (line numbers, status bar, tab strip, scrollbar, pane headers). Space locks; the chord again dismisses.
  - Locks are saved in the session. Everything is configurable via `ui.summon.*` and the settings rows.
  - The TUI toggles instead of holding.
- #2273 layout-override diff prompt, shown on a layout switch and once at first launch. "Use layout" removes the explicit config lines and keeps a `.bak`. The marker is `~/.serenity/layout-override-reviewed`.
- #2274 End then Down/Up skipped a row in wrapped text (`NavigationGeometry` affinity).
- #2275 black first frame when a startup modal sits over the start page (`RendererCursorOverlay` repaint).
- #2276 conventional layout: the last row drew over the status row (pane clip).
- #2277 stale content left below the document end after PageDown in minimal: the vacated band was never published to the screen. Owner confirmation on the laptop is still pending.

## 3. Waiting on the owner

1. **Laptop check:** on `master` (62f024e7b or later), the owner should confirm that PageDown to the end of their RTF novel in Prose mode leaves the area below the last chapter blank.
2. **rc.3 tag:** if that check passes, the owner pushes `v1.0.0-rc.3` on that commit:
   `git tag -a v1.0.0-rc.3 <sha> -m "Serenity 1.0.0-rc.3" && git push origin v1.0.0-rc.3`.
   Earlier tags (rc.1, rc.2) were never pushed to origin; only `desktop-latest` exists remotely.
3. **Hardware checks still to do:**
   - Hold-to-summon flicker: the 12 ms autorepeat filter, `SummonChordTracker.RepeatGapMillis`.
   - The first-launch layout prompt, seen properly. Restore `~/.serenity/config.conf.pre-rc3` and delete the marker to see it again.

## 4. Queue (not started; start from here as slots free)

- **Modal message lines don't wrap in the GUI.** They get cut off and overlap the buttons (seen in the #2275 investigation).
- **`ConfigManager.settingKeysIO` swallows read errors** (`Try(...).getOrElse(Set.empty)`, around lines 231-233). Because the marker is written first, an unreadable config means no layout prompt and no retry.
- **Cold start takes about 4.6 s to first paint on the laptop,** about 3 s of it before "Starting Serenity". First check whether that was `sbt run` overhead.
- **word-sample.rtf in the conventional layout:** a 3 px descender spill on a heading "y" (probably #1839). It was excluded from `PageDownRichDocumentFramesSpec`, so fix it and re-include the case.
- **Find out why the scrollbar-on path avoided the #2277 bug.**
- **Older items** (details in the log):
  - alt-mode menu bar blank frame on show;
  - WrappedLineCache retention (paneScroll wraps up to 2048 lines);
  - translucent-panel repaint optimisation, then the theme-alpha PR (branch `-theme-alpha`, a1447f383);
  - kerning: wrap `ShapingBarriers.probed` in `SettledProbe`;
  - Windows smoke-wait;
  - #1908 line-hash;
  - #1900 Step C;
  - a rich-document-with-images footprint fixture;
  - `EscLivenessSpec` timing under load;
  - `FootprintBenchmarksSpec` flaky under heap pressure.
- **Closed, not reproduced:** the TUI caret off-by-one at a hung wrap space. Viewport following was verified across 26 runs.

## 5. Laptop

- **Machine:** NixOS, Hyprland/XWayland, i7-8550U, UHD 620. The owner's config has these set explicitly: `editor.line_numbers=false`, `editor.pane_headers=false`, `status.placement=pinned`.
- **Laptop agent session:** https://claude.ai/code/session_01NNpCYA52hyiMw7omYfjEdV.
  - Rules for it: additive worktrees only, no sudo, `systemd-inhibit`, one sbt or java process at a time, and back up and restore `config.conf` (verify with md5).
  - For long runs, use `systemd-run --user` so the task layer doesn't kill them.
- **Results come back by pushing a branch**, which this side then reads. Examples: `claude/bench-results-jobs-5-6-7` and `claude/laptop-diag-startup-pagedown`, the latter with screenshots of the stale chapter.
- **Connector problem:** in the previous session the `claude-code-remote` connector kept timing out, so it couldn't message the laptop or read its transcript. An approval tool appeared that suggests the owner may need to allow this session to reach other sessions, which limits it to trusted devices.

## 6. Schedule

- Routine `trig_01VNRgJ1pqhrprd5VL8tjKuW`, "Serenity: hourly check-in (lean)", fires into the OLD session (session_01FFaijrmcykoATN664JcAzY) at about :50 each hour.
- **In the new session, first create its own hourly routine (bound to the new session), then disable or delete the old one** with `update_trigger` or `delete_trigger`. If the connector is down, use the claude.ai Routines UI. The old session couldn't edit it because the connector was down.
- **The routine prompt to recreate:**
  > Hourly check-in, lean mode. Read the latest section of the AGENTS log. 1) ReadNotifications; red CI or merge conflicts on my open PRs go to at most ONE Sonnet triage agent. 2) At most 2 Sonnet implementation agents, never Opus; start one only when a slot frees, from the queue; resume any stopped by usage limits. 3) Integrate finished worktrees as PRs. 4) Append 1-2 lines to the log. 5) If nothing changed, reply in one line and do nothing else.

  Keep the log in the new session's scratchpad. Seed it from `handover/AGENTS-log.md`.
- **No PR subscriptions are active.** Subscribe after opening each new PR.

## 7. Environment caveats seen

- **Network allowlist:** for example, `code.visualstudio.com` returned a proxy 403. GitHub, Maven and npm are allowed.
- **GitHub MCP drops intermittently,** and `git fetch` can time out silently. Re-verify merge state with `git ls-remote origin refs/heads/master` or the PR itself before reporting it.
