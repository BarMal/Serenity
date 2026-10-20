# Purity allowlist review, entry by entry

Research and proposals only. **Nothing here is implemented**, and each entry waits for the owner's sign-off before any
code is written (#2350).

`ArchitectureChecks.PurityAllowlist` (`project/ArchitectureChecks.scala:304`) names ten declarations that the
declaration-level purity rules (`project/PurityChecks.scala`, #1677, #2324) catch but that were judged unable to move
into runtime-owned state. This page revisits every one for a redesign. It builds on the shorter page
[`docs/allowlist-review.md`](../allowlist-review.md), which `ArchitectureChecks.scala:302`, `PurityChecks.scala:327` and
`DEVELOPMENT.md:130` point at; where this page disagrees with that one, it says so under **Corrections**.

## How to read this

- Every entry gives: what it is, why it is impure or exempt (with `file:line`), one or two redesigns with their
  trade-offs including performance risk, and a recommendation (or **keep**, with a justification).
- Line numbers are from `origin/master` at the time of writing.
- **Nothing was measured.** Performance statements are read off the code and the #1277 history the code comments cite.
  Where an inference carries weight it is marked *Caution, here be imagine dragons*. Any redesign that touches a hot path
  needs the ABBA protocol ([performance-benchmarks.md](../performance-benchmarks.md#abba-protocol), `bench/abba.sh`)
  before it merges.
- Size is the estimated size of the redesign PR: **small** is one file plus its spec; **medium** is a handful of files
  and a signature change along one call chain; **large** is a cross-cutting signature or ownership change.

## Summary

| # | Entry | Size | Recommendation |
| --- | --- | --- | --- |
| S1 | `FontFirstUse.NoContexts` | small | Redesign: drop the concurrent-map sentinel |
| S2 | `TuiConsoleLogFilter.suppressConsole` | small | Redesign: attach the filter from `Main` with its `LogRouting` |
| M1 | `UiPresetStore.inProcessLocks` | medium | Redesign, but first a failing test: the lock may not exclude (see entry) |
| M2 | `ParagraphMeasurement.threadLocalCharacterBreaks` | medium | Redesign: pass a scratch through the wrap pass |
| M3 | `TextLayoutSnapshot.threadLocalLineBreakIterator` | medium | Same redesign as M2 |
| L1 | `TextEditing.threadLocalBreakIterator` | large | Keep until a benchmark can gate it |
| L2 | `TextEditing.threadLocalWordBreakIterator` | large | Keep, with L1 |
| L3 | `RopeCharacterSource.lastUsed` | large | Keep, with L1 |
| L4 | `TextCounts.threadLocalWords` | large | Keep, with justification |
| L5 | `TextCounts.threadLocalCharacters` | large | Keep, with L4 |

Order I would take them in: S1, S2, then M1's test, then M2 and M3 together. L1 to L5 are not worth starting before the
two benchmarks named under L1 exist.

## Corrections to the earlier page

- **`FontFirstUse.NoContexts` is not a one-line `Map.of()`.** The sentinel is the default for
  `settled.getOrDefault(font, NoContexts)` (`FontFirstUse.scala:24`) and `settled`'s values are
  `ConcurrentHashMap[...]` (`:20`), so a `Map.of()` default only compiles if the value type is widened too. See S1.
- **`UiPresetStore` may have a bigger problem than the global.** A `ReentrantLock` is owned by a thread, and it is locked
  and unlocked in separate `IO.blocking` steps. See M1.
- **The "single owner" precondition for M1 is not met today:** three places build a store (see M1).

---

## Small

### S1. `FontFirstUse.NoContexts`

- **Where:** `src/main/scala/com/serenity/ui/renderer/FontFirstUse.scala:45`; read at `:24`.
- **What it is:** an empty `ConcurrentHashMap` used only as `getOrDefault`'s miss value, so the common path (font already
  settled, or not yet seen) allocates nothing.
- **Why it is flagged:** the checker matches the token `ConcurrentHashMap` in an `object` (`PurityChecks.scala:40`). It
  is never written, so it carries no state; the entry is exempt on that ground.
- **Options:**
  1. *Immutable empty map.* Widen `settled` to `ConcurrentHashMap[Font, java.util.Map[FontRenderContext, java.lang.Boolean]]`
     (`:20`) and use `java.util.Map.of()` as the default. The inner map is still created as a `ConcurrentHashMap` at
     `:35`, so writes are unchanged. `Map` is not in the checker's mutable list (`PurityChecks.scala:30`), so the entry
     goes. Cost: same as today (one shared instance, no allocation). Risk: a type-widening ripple, confined to this
     file.
  2. *No sentinel.* `Option(settled.get(font)).exists(_.containsKey(frc))`. Reads naturally and is purest. *Caution, here
     be imagine dragons:* `Option(...)` allocates a `Some` on a hit; escape analysis probably removes it, but this is
     the first check of every `settle`, which about a dozen measuring and shaping paths call (for example
     `GlyphAdvances.scala:117`, `ShapedTextCache.scala:187`, `TextLayoutSnapshot.scala:534`), so it should be
     benchmarked rather than assumed.
- **Recommendation:** option 1. It keeps the zero-allocation path exactly and removes the entry. Option 2 only if
  `FontFirstUse` is reworked for ownership anyway (its `shared` instance is a baseline entry, `FontFirstUse.scala:50`).
- **Gate:** the `RenderBenchmarks` and `FontFallbackColdFrameSpec` rows (`src/test/scala/com/serenity/perf`). I found no
  unit spec for `FontFirstUse` itself, only the `FontFirstUseRepro` harness, so the redesign would add one.

### S2. `TuiConsoleLogFilter.suppressConsole`

- **Where:** `src/main/scala/com/serenity/diagnostics/TuiConsoleLogFilter.scala:40`; set at `:46`, read at `:48` and in
  `decide` (`:28-29`); configured from `src/main/scala/Main.scala:130`; wired in `src/main/resources/logback.xml:7`.
- **What it is:** a static `AtomicBoolean` that tells the console appender's filter to deny every event in TUI mode
  (#1215), because stdout is the terminal surface there.
- **Why it is flagged:** logback builds the filter by reflection from `logback.xml`, so no `LogRouting` value can be
  passed to it (`:33-39` says so). Some channel from `Main` into the filter is needed; today it is a static.
- **Options:**
  1. *Attach the filter from `Main`.* Remove `<filter>` from the XML. In `Main.launch`, after logback initialises and
     before the first application log, get the `LoggerContext`, look up the `STDOUT` appender and `addFilter` a filter
     instance constructed with the `LogRouting` in hand. The filter becomes a plain class with a constructor argument
     and no static. Risk: logback must be initialised before the first event is attached to, which in practice means
     `Main` calls `LoggerFactory.getILoggerFactory` itself; no application event is logged between initialisation and
     `addFilter`, so none can leak. Tests that boot logback without `Main` get no filter (their lines print, as with a
     GUI launch). `TuiConsoleLogFilterSpec` (`:21-30`) would be rewritten around the instance.
  2. *Park the value in logback's own context.* `Context.putObject` / `getObject` exist in logback-core 1.5.38 (checked
     with `javap` on the jar in the build's dependency cache). `Main` would put the `LogRouting` there and the XML-built
     filter would read it from `getContext`. This moves the global into logback rather than removing it, and trades a
     typed static for a string key and a cast; it is the smaller edit but the weaker design.
- **Performance risk:** none on a hot path; `decide` runs per log event and reads one field either way.
- **Recommendation:** option 1. It is the one entry with a real structural fix, and the fix is logging configuration
  at the application edge, where `Main` already lives.

---

## Medium

### M1. `UiPresetStore.inProcessLocks`

- **Where:** `src/main/scala/com/serenity/ui/presets/UiPresetIndex.scala:200` (map) and `:202-203` (lookup); used by
  `withExclusiveMutationLock` at `:80-90`; the store class starts at `:75`.
- **What it is:** one `ReentrantLock` per preset-lock-file path, shared by every `UiPresetStore` in the process, taken
  before a `FileChannel.lock` on the sibling `.lock` file (`:86-88`).
- **Why it is flagged:** the map is process-wide mutable state (`ConcurrentHashMap`, `PurityChecks.scala:40`). The
  reason it exists is that two stores for one file must not both reach the OS lock: `FileChannel.lock` throws
  `OverlappingFileLockException` when the same JVM already holds an overlapping lock. *Caution, here be imagine
  dragons:* that is from my memory of the `FileChannel` Javadoc; I have not run it here.
- **Possible bug, found while reading (not reproduced):** the lock is acquired in `IO.blocking(processLock.lock())`
  (`:82`), `operation` runs, and the release is a separate `IO.blocking(processLock.unlock())` (`:90`). A
  `ReentrantLock` belongs to the thread that locked it, and `IO.blocking` steps may run on different pool threads.
  *Caution, here be imagine dragons:* two things could follow. `unlock` on another thread throws
  `IllegalMonitorStateException` and leaves the lock held; or a second fiber's `lock()` landing on the thread that
  still holds it re-enters the lock and does not wait, so the in-process exclusion does nothing and the OS lock's
  `OverlappingFileLockException` is the only guard. I did not test either. A concurrency test with two stores on one
  path would show it.
- **Who builds a store today:** `StartupRecovery.scala:60-63`, `TuiRuntime.scala:182` (default argument),
  `StateManager.scala:211` (default argument). So "one store per runtime" is not guaranteed.
- **Options:**
  1. *Cooperative keyed mutex, runtime-owned.* A `Ref[IO, Map[Path, Mutex[IO]]]` (or `Semaphore`) owned by one object
     that all three construction sites share. Cats Effect mutexes are not thread-owned, so the ownership question goes
     away, and fibers wait without blocking a thread. Cost: the registry must be plumbed to the three sites; the
     guarantee holds within one registry only, so two runtimes in one process sharing a file would again rely on the OS
     lock.
  2. *Keep the process-wide map, replace the lock type.* Same global, but the value becomes a `Semaphore[IO]`/`Mutex[IO]`
     created through an `IO` accessor, which fixes the ownership problem without a plumbing change. The entry stays on
     the allowlist, now for a correct reason.
- **Performance risk:** none; preset writes are rare and already do file IO.
- **Recommendation:** first write the failing concurrency test (small, no design needed). Then option 2 if the owner
  wants the bug fixed now with a minimal diff, option 1 if the owner wants the global gone. If only one runtime ever
  writes a given file, option 1 is strictly better.

### M2. `ParagraphMeasurement.threadLocalCharacterBreaks`

- **Where:** `src/main/scala/com/serenity/ui/layout/ParagraphMeasurement.scala:85`; used in `boundariesOf` (`:116-126`),
  reached from `apply` (`:95`) and `graphemeBoundaryOffsets` (`:113`). Callers of the latter: `:40` and
  `SegmentMeasurement.scala:26`.
- **What it is:** a per-thread character `BreakIterator`, `setText`-ed per paragraph window to collect grapheme
  boundaries on the wrapping path.
- **Why it is flagged:** `ThreadLocal` in an object. `BreakIterator` is mutable, not thread-safe and costly to build
  (the #1277 history).
- **Options:**
  1. *Pass a scratch.* A small `BoundaryScratch` value (the iterator, created per wrap pass) threaded from the wrap entry
     point to `boundariesOf`. The call chain here is short (`ParagraphMeasurement.apply` and `graphemeBoundaryOffsets`
     have few callers, listed above). Cost: a new parameter on those entry points; the scratch must stay on one thread
     for a pass, which holds because wrapping is synchronous. Risk: if a pass is created per line instead of per
     paragraph or frame, the allocation returns. *Caution, here be imagine dragons:* I have not measured how often
     `ParagraphMeasurement.apply` runs per frame.
  2. *Allocate per call.* Remove the thread-local and build an iterator in `boundariesOf`. Simplest, but it is what
     #1277 moved away from for the line iterator (`TextLayoutSnapshot.scala:513-518`); character instances are cheaper
     than line instances but not free. Not recommended without a measurement.
- **Recommendation:** option 1, done together with M3 so a wrap pass owns one scratch with both iterators.
- **Gate:** `LayoutSnapshotBenchmarks` and the typing-burst specs (`TypingBurstLatencySpec`), through `bench/abba.sh`.

### M3. `TextLayoutSnapshot.threadLocalLineBreakIterator`

- **Where:** `src/main/scala/com/serenity/ui/layout/TextLayoutSnapshot.scala:519`; used in `wordBoundarySegmentLength`
  (`:525-531`); its only caller is `:489`.
- **What it is:** a per-thread line-break `BreakIterator` reused across wrapped lines. The code comment (`:513-518`) says
  building one per wrapped line measurably slowed documents with many lines (#1277).
- **Why it is flagged:** as M2.
- **Options:**
  1. *Pass the scratch from M2.* There is a single caller, so this is the smallest part of the change. The scratch holds
     both the character and the line iterator.
  2. *Pass only the line iterator.* Same shape, but a separate parameter next to M2's; two scratch values to thread.
- **Performance risk:** the line instance is the expensive one to construct, so the pass must create it once per pass,
  not per line. This is the one place where option 1's allocation concern is real, and why M2 and M3 go together.
- **Recommendation:** option 1, with M2.

---

## Large

L1 to L3 are one redesign and L4 to L5 are another; they are separate entries because each has its own declaration and
reason in the allowlist.

### L1. `TextEditing.threadLocalBreakIterator`

- **Where:** `src/main/scala/com/serenity/text/TextEditing.scala:232`; read at `:245`
  (`graphemeBreakIterator`). The cache class is `BreakIteratorCache` (`:223-230`).
- **What it is:** a per-thread grapheme `BreakIterator` paired with the identity anchor of the `CharacterSource` it was
  last `setText`-ed onto, so repeated boundary questions about one unchanged rope or string skip `setText`
  (`forSource`, `:226-230`). The comment at `:208-219` records that building the iterator per query measurably regressed
  find/replace over large documents.
- **Why it is flagged:** `ThreadLocal` in an object, and a cache whose cost depends on which thread ran last.
- **Reach (inference from grep, not a call-graph):** about seven files use the `TextEditing` boundary API directly and
  about ten call the rope extension methods that wrap it (`RopeCharacterSource.scala:51-70`). `FindSearch.results`,
  `TextLayoutSnapshot` and `CharacterRenderer` are named as callers in the comments at `:208-219`.
- **Options:**
  1. *Pass a scratch value.* Every boundary function takes a `BoundaryScratch` (grapheme iterator, word iterator,
     adapter, anchors). Purest. Cost: every caller up to the reducers and commands that move the cursor needs one;
     either it is built per call (reintroducing the `setText` cost the anchor avoids) or it is held somewhere, and the
     only place that outlives a call is state, which is not a home for an ICU object.
  2. *Hang the iterator off the `CharacterSource`.* The source is already what callers hold across a scan (`FindSearch`),
     so it could own a lazily created iterator and the anchor test becomes `this eq last`. This helps scans and does
     nothing for per-keystroke calls, which build a fresh adapter each time unless `RopeCharacterSource.of` (L3) remembers
     the last one, which is the thread-local again.
- **Performance risk:** high. This is the per-keystroke path and the find path. *Caution, here be imagine dragons:* I
  could not find a benchmark that isolates it in the ABBA suite: `PickerAndGraphemeBenchmark` is a standalone
  `runMain` (`PickerAndGraphemeBenchmark.scala:14`), and `FindReplaceBenchmarks` covers find but I did not confirm it
  reaches these iterators.
- **Recommendation: keep, with justification.** A scratch parameter has no natural owner on the keystroke path, and
  option 2 only helps scans. Revisit after (a) a per-keystroke boundary row exists in the ABBA suite and (b) M2/M3 have
  shown what a scratch value looks like in practice. If the owner wants this tried sooner, option 2 for scans only is
  the safe slice.

### L2. `TextEditing.threadLocalWordBreakIterator`

- **Where:** `src/main/scala/com/serenity/text/TextEditing.scala:237`; read at `:164` (`isWholeWordRange`) and `:248`.
- **What it is:** the word-boundary twin of L1, kept separate so word motion's grapheme questions do not evict its text
  (`:235-236`).
- **Why it is flagged and options:** as L1; it is the same cache class. Any scratch value would hold both.
- **Recommendation: keep, with L1.** It cannot move independently, because the second cache exists to avoid the first
  one's evictions.

### L3. `RopeCharacterSource.lastUsed`

- **Where:** `src/main/scala/com/serenity/rope/RopeCharacterSource.scala:35`; read at `:42` in `of`, which backs the
  rope extension methods at `:51-70`.
- **What it is:** a per-thread memo of the last `RopeCharacterSource` made for a rope. Its identity is the anchor L1/L2
  compare, so the memo is what lets a keystroke's several boundary questions share one `setText`
  (comment at `:37-40`). The adapter also keeps its own per-instance leaf cache (`cachedChunk`, `:18`).
- **Why it is flagged:** `ThreadLocal`.
- **Options:**
  1. *Fold into the L1 scratch.* The memo disappears when the caller holds the adapter. Depends on L1.
  2. *Cache the adapter on the `Rope`.* Ties a mutable cache to a persistent value; the earlier page rejected it and I
     agree, because it gives `Rope` equality-by-structure but cost-by-history.
- **Recommendation: keep, with L1.** It cannot be fixed on its own.

### L4. `TextCounts.threadLocalWords`

- **Where:** `src/main/scala/com/serenity/text/TextCounts.scala:42`; read at `:66`.
- **What it is:** a per-thread word `BreakIterator` used by `TextCounts.of` (`:62-69`) to count words and characters.
  A comment at `:41` notes it is separate from `TextEditing`'s iterators because counting sets arbitrary strings, which
  would evict their cached text.
- **Why it is flagged:** `ThreadLocal`. Its sibling `threadLocalSegmentations` (`:49`) is already a baseline entry
  (`project/architecture-baseline.tsv:22`), so this file's thread-local story is wider than the allowlist.
- **What makes it hard:** the callers are not loops that could own a scratch. `TextCounts.of` is reached through
  `TextSummary.of` and `join` (`TextCounts.scala:129-155`), which `Rope.scala:293` evaluates as a `lazy val`
  (`override lazy val textSummary`) on a leaf, and `Rope.scala:352` folds over children. A `lazy val` takes no
  parameters, and the thread that forces it is whichever asked first.
- **Options:**
  1. *Give `Rope` a text-services context.* Every rope construction or summary would take a scratch or a factory. This
     is the whole rope API; it is out of proportion to the gain.
  2. *Hand-rolled counter without `BreakIterator`.* A bespoke UAX #29 word and grapheme counter has no mutable
     iterator, so no scratch at all. Correctness risk is high (the rules are large; CJK and emoji sequences), and it
     would have to match `TextEditing`'s behaviour exactly or word counts and cursor motion would disagree.
- **Performance risk:** high for option 1 (rope edits), unknown for option 2.
- **Recommendation: keep, with justification.** The state is confined to its thread, never shared, never observable
  (the answers are pure functions of the text), and the only callers that could pass a scratch are inside a persistent
  data structure's lazy summaries. `TextCounts.countSegmentations` (`:56-60`) already lets specs prove that edits
  segment nothing.

### L5. `TextCounts.threadLocalCharacters`

- **Where:** `src/main/scala/com/serenity/text/TextCounts.scala:45`; read at `:64`.
- **What it is:** the character-instance twin of L4, also passed into `countWords` for per-character scripts (`:93`).
- **Recommendation: keep, with L4.** Same reasoning and same blockers.

---

## Open questions for the owner

1. **M1:** do you want the possible lock-ownership problem tested first? It needs no design decision.
2. **S2:** is moving the console filter from `logback.xml` to `Main` acceptable? It changes where logging is configured.
3. **M2/M3:** is a `BoundaryScratch` parameter on the wrap entry points acceptable, given it adds a parameter along one
   call chain?
4. **L1 to L5:** should a per-keystroke boundary benchmark be added before any of these are revisited, or are they
   closed as "keep"?
5. **This page vs `docs/allowlist-review.md`:** the old page is pointed at by the build files. Should this page replace
   it (and those pointers move), or should the two be merged? I have not touched the old page.

When an entry is redesigned, delete it from `PurityAllowlist` in the same change: the check fails on an entry that no
longer matches a declaration, so a stale reason cannot linger.
