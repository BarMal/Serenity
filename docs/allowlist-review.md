# Purity allowlist review

`ArchitectureChecks.PurityAllowlist` (`project/ArchitectureChecks.scala`) names the declarations the purity rules (`project/PurityChecks.scala`) catch but that were judged unable to move into runtime-owned state. Each has a reason there. This page revisits every entry for a possible redesign. **Nothing here is implemented**: it records alternatives, trade-offs and a recommendation so that each can be taken on, or declined, in its own PR.

Unless a line says "measured", performance statements below are inference from reading the code and from the perf history behind `#1277`. They are marked *Caution, here be imagine dragons* where they carry weight. Any redesign needs the ABBA protocol in [performance-benchmarks.md](performance-benchmarks.md#abba-protocol) before it merges.

## The thread-confined `BreakIterator` scratch (six entries)

| Entry | File |
| --- | --- |
| `TextCounts.threadLocalWords`, `TextCounts.threadLocalCharacters` | `text/TextCounts.scala` |
| `TextEditing.threadLocalBreakIterator`, `TextEditing.threadLocalWordBreakIterator` | `text/TextEditing.scala` |
| `ParagraphMeasurement.threadLocalCharacterBreaks` | `ui/layout/ParagraphMeasurement.scala` |
| `TextLayoutSnapshot.threadLocalLineBreakIterator` | `ui/layout/TextLayoutSnapshot.scala` |

**Why it is shared state at all.** An ICU/JDK `BreakIterator` is mutable and not thread-safe, and `setText` on a long string is not free. Each of these keeps one per thread so a hot loop (boundary questions on every keystroke, wrapping a paragraph) neither allocates one nor contends for one. The `TextEditing` pair also remembers the identity of the text last set, which skips `setText` entirely when consecutive questions concern the same rope. The state is never observable: it is a scratch buffer, and the answers are pure functions of the text.

**What is impure about it.** It is process-wide mutable state reached through a hidden global, so a function's cost (not its result) depends on which thread ran it last, and a test cannot see or reset it.

| Alternative | What changes | Trade-offs |
| --- | --- | --- |
| A. Pass the iterator (or a small `BoundaryScratch` value) as a parameter | Callers that loop (a wrap pass, a motion command) create one scratch and thread it through the calls | Purest and easiest to reason about; the cost is a wider signature on `TextEditing`, `ParagraphMeasurement` and the layout entry points, which have many callers. A scratch that crosses an `IO` boundary is no longer thread-confined unless the caller keeps it on one fiber. |
| B. A runtime-owned pool (`Resource`-scoped, borrowed per call) | `RenderCaches`/the runtime owns N iterators; a call borrows and returns one | Keeps allocation off the hot path and removes the global. A borrow is an atomic hand-off where a thread-local read is nearly free. #1277 found that moving this state was a perf problem, so expect the borrow to show up on the keystroke path. |
| C. `IOLocal` | The scratch rides in fiber-local state | Follows the fiber, not the thread, which is the correct confinement under Cats Effect. These functions are not in `IO`, so it would force `IO` into pure text code, against the functional-core rule. Rejected on layering grounds. |
| D. Allocate per call | Delete the cache | Simplest, and the allocation is small next to the shaping work, but `setText` on a long rope is the expensive part and the identity anchor exists to skip it. *Caution, here be imagine dragons: the saving from the anchor is why #1277 resisted moving this; I have not re-measured it.* |

**Recommendation: A for the two hot callers that already loop (wrapping in `ParagraphMeasurement` and `TextLayoutSnapshot`), keep the others on the allowlist.** Pass one scratch per wrap pass and leave `TextEditing`'s per-keystroke path alone until a benchmark shows a pooled scratch is free. Do not take B or C. Until A lands, the allowlist is the honest answer.

## `RopeCharacterSource.lastUsed`

`rope/RopeCharacterSource.scala`. A per-thread memo of the adapter last made for a rope. Its identity is the anchor the `TextEditing` `BreakIterator` caches compare, so it only has meaning alongside them.

| Alternative | Trade-offs |
| --- | --- |
| A. Make the adapter part of the scratch value passed in (see above) | The memo disappears: the caller that holds the scratch holds the adapter for the rope it is working on. No global, no thread-local. Depends on the `BreakIterator` redesign above. |
| B. Cache the adapter on the `Rope` | Ties a mutable cache to a persistent value and makes `Rope` equal-by-structure but not by cost; rejected. |
| C. Drop the memo | A fresh adapter per call defeats the anchor, so every boundary question would `setText` again. |

**Recommendation: fold it into the `BreakIterator` redesign (A) and keep it allowlisted until then.** It cannot be fixed on its own.

## `TuiConsoleLogFilter.suppressConsole`

`diagnostics/TuiConsoleLogFilter.scala`. logback builds the filter by reflection from `logback.xml`, so a value cannot be injected, and some global channel from `Main.launch` is unavoidable (the file says so, #1669).

| Alternative | Trade-offs |
| --- | --- |
| A. Keep a flag, but make it a one-shot cell (`Deferred`/write-once) set by `configure` | The type then says "set once, never cleared", which is what the code already assumes. Still global; reads cost the same. |
| B. Configure logback programmatically from `Main` (`LoggerContext`, a filter instance we construct and hand over) | Removes the static: the filter is built with the `LogRouting` in hand. Needs the logback config out of XML for the console appender, and every test that boots logback to follow. |
| C. Install the filter after the appender exists via `addFilter` | Same effect as B with less XML change; the window before installation can leak early lines onto the terminal, which is exactly what `configure`'s ordering exists to prevent. |

**Recommendation: B, if the XML-to-code move is wanted for its own sake; otherwise keep the entry.** It is the one entry with a real structural fix, and the fix is a logging-configuration change rather than a state-management one.

## `UiPresetStore.inProcessLocks`

`ui/presets/UiPresetIndex.scala`. One `ReentrantLock` per preset file, shared by every `UiPresetStore` that names it, so two stores in one process cannot write the same file at once (a sibling `.lock` file is also taken through `FileChannel.lock` for other processes). *Caution, here be imagine dragons: the in-process lock probably exists because a second `FileChannel.lock` in one JVM throws `OverlappingFileLockException`; I have not confirmed that was the reason.*

| Alternative | Trade-offs |
| --- | --- |
| A. Give each runtime one `UiPresetStore` and make it the only writer | Then a per-instance `Mutex[IO]` suffices and the map goes. True today only if nothing constructs a second store for the same path; the `apply(path)` factory suggests callers do (tests, tooling). |
| B. A `Mutex[IO]` keyed through a runtime-owned `Ref[IO, Map[Path, Mutex[IO]]]` | The map becomes runtime state and the lock becomes cooperative (fibers wait without blocking a thread), which is the better fit for Cats Effect. Requires the store's blocking file work to run inside `IO`, and the cross-instance guarantee holds only within one runtime. |
| C. Rely on the file lock alone | Removes in-process state, but an in-process contender then spins or fails on the OS lock instead of queueing. |

**Recommendation: B, after A's precondition is checked.** If a single runtime owns all preset writes, `Mutex[IO]` removes the global and is strictly better; if two runtimes can share a file, the process-wide map is the correct design and the entry stays.

## `FontFirstUse.NoContexts`

`ui/renderer/FontFirstUse.scala`. An empty `ConcurrentHashMap` used only as the `getOrDefault` miss value, so the common path allocates nothing. It is never written.

| Alternative | Trade-offs |
| --- | --- |
| A. Replace the sentinel with `Option`/pattern match on `settled.get(font)` | No shared instance, no mutable type in an object; one `null` check or `Option` per lookup, which `ConcurrentHashMap.get` already returns as `null`. |
| B. An immutable empty `java.util.Map` (`Map.of()`) | Same cost, no `ConcurrentHashMap` token, and it cannot be written even by mistake. |

**Recommendation: B, now; it is a one-line change.** The sentinel never needed to be a concurrent map. Take A only if `FontFirstUse` is reworked for ownership anyway (it is itself a baseline entry, `FontFirstUse.shared`), since A reads more naturally once `settled` is instance state.

## Summary

| Entry | Recommendation | Blocked by |
| --- | --- | --- |
| `BreakIterator` scratch (six) | Pass a scratch to the looping callers; allowlist the rest | benchmarks showing no keystroke-path loss |
| `RopeCharacterSource.lastUsed` | Fold into the scratch redesign | the row above |
| `TuiConsoleLogFilter.suppressConsole` | Programmatic logback configuration | decision to leave logback XML |
| `UiPresetStore.inProcessLocks` | Runtime-owned `Mutex[IO]` map | whether two runtimes can share a preset file |
| `FontFirstUse.NoContexts` | `Map.of()` | nothing |

When an entry is redesigned, delete it from `PurityAllowlist` in the same change: the check fails on an allowlist entry that no longer matches a declaration, so a stale reason cannot linger.
